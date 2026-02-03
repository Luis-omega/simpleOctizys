module Ast.Inference where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State, get, put)

-- \* Pretty imports
import Prettyprinter
  ( Pretty (pretty)
  , (<+>)
  )

-- \* JSON imports
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

-- \* Internal imports

import Ast.Combinators
  ( annotate
  , application
  , function
  , letExp
  , parameterWithSymbol
  , recordWithSymbols
  , selectionWithField
  )
import Ast.Context (Context)
import qualified Ast.Context as Context
import Ast.Expression
  ( Definition (..)
  , Expression
    ( Annotation
    , Application
    , BoolLiteral
    , ExpressionVariable
    , Function
    , IntLiteral
    , Let
    , Record
    , Selection
    )
  , getParameterSymbol
  , getParameterType
  )
import Ast.Symbol (FieldName, Symbol, makeSymbol)
import Ast.Type (SimpleType (..))


data Constraint
  = EqConstraint SimpleType SimpleType
  | HasFieldConstraint SimpleType FieldName
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Constraint


instance Pretty Constraint where
  pretty (EqConstraint x y) =
    pretty x
      <+> pretty '~'
      <+> pretty y
  pretty (HasFieldConstraint ty fieldName) =
    pretty @String "HasField"
      <+> pretty ty
      <+> pretty fieldName


freshVariable
  :: State Int :> es
  => Eff es Symbol
freshVariable = do
  currentValue <- get
  put (currentValue + 1)
  pure (makeSymbol $ show currentValue)


freshTypeVar
  :: State Int :> es
  => Eff es SimpleType
freshTypeVar =
  TypeVariable <$> freshVariable


generateDefinitionVarsTypes
  :: Error String :> es
  => State Int :> es
  => [Definition]
  -> Eff es [((Symbol, SimpleType, Expression), Maybe Constraint)]
generateDefinitionVarsTypes defs = do
  mapM
    ( \def -> do
        ty <- freshTypeVar
        case def.annotation of
          Just someType ->
            pure
              ( (def.name, ty, def.value)
              , Just (EqConstraint ty someType)
              )
          Nothing ->
            pure
              ( (def.name, ty, def.value)
              , Nothing
              )
    )
    defs


inferDefinitions
  :: Error String :> es
  => State Int :> es
  => [Definition]
  -> Context
  -> Eff es (Context, [(Symbol, SimpleType)], [Definition], [Constraint])
inferDefinitions defs context = do
  checkResults <- generateDefinitionVarsTypes defs
  let
    variablesTypes = fst <$> checkResults
    annotationConstraints = mapMaybe snd checkResults
  augmentedContext <-
    Context.addExpressionVars
      ((\(x, y, _) -> (x, y)) <$> variablesTypes)
      context
  results <-
    mapM
      ( \(name, assignedType, value) ->
          do
            (_, inferredType, annotatedExp, generatedConstraints) <-
              infer value augmentedContext
            let
              newConstraint = EqConstraint assignedType inferredType
              newConstraints = newConstraint : generatedConstraints
            pure ((name, annotatedExp, inferredType), newConstraints)
      )
      variablesTypes
  let
    newConstraints = annotationConstraints <> concatMap snd results
    typedSymbols = (\((n, _, t), _) -> (n, t)) <$> results
    newDefinitions = (\((n, e, t), _) -> Definition n (Just t) e) <$> results
  pure (augmentedContext, typedSymbols, newDefinitions, newConstraints)


infer
  :: Error String :> es
  => State Int :> es
  => Expression
  -> Context
  -> Eff es (Context, SimpleType, Expression, [Constraint])
infer expression context =
  case expression of
    IntLiteral _ -> pure (context, IntType, annotate expression IntType, [])
    BoolLiteral _ -> pure (context, Boolean, annotate expression Boolean, [])
    ExpressionVariable symbol -> do
      ty <- Context.findExpressionVariable symbol context
      pure (context, ty, annotate expression ty, [])
    Function param result -> do
      paramTypeVar <-
        maybe freshTypeVar pure (getParameterType param)
      let
        paramSymbol = getParameterSymbol param
      (ctx, sty, annotatedBody, constraints) <-
        infer
          result
          ( Context.addTypeToExpression context paramSymbol paramTypeVar
          )
      let
        inferredType =
          Arrow
            paramTypeVar
            sty
        annotatedParam = parameterWithSymbol paramSymbol (Just paramTypeVar)
        annotatedExpression = annotate (function annotatedParam annotatedBody) inferredType
      pure
        ( ctx
        , inferredType
        , annotatedExpression
        , constraints
        )
    Application f arg -> do
      resultType <- freshTypeVar
      (_, fType, fAnnotated, constraintsF) <- infer f context
      (_, argType, argAnnotated, constraintsArg) <- infer arg context
      let newArrow = Arrow argType resultType
          newConstraints =
            EqConstraint fType newArrow
              : ( constraintsF
                    <> constraintsArg
                )
          annotatedExpression = annotate (application fAnnotated [argAnnotated]) resultType
      pure (context, resultType, annotatedExpression, newConstraints)
    Record fs -> do
      items <-
        mapM
          ( \(name, x) -> do
              (_, ty, ex, css) <- infer x context
              pure ((name, ty, ex), css)
          )
          (Map.toList fs)
      let
        fieldTypes = (\((n, t, _), _) -> (n, t)) <$> items
        annotatedExpressions = (\((n, _, e), _) -> (n, e)) <$> items
        recordWithNotes = recordWithSymbols annotatedExpressions
        constraints = snd <$> items
        recordTy = RecordType (Map.fromList fieldTypes)
        annotatedExpression = annotate recordWithNotes recordTy
      pure (context, recordTy, annotatedExpression, concat constraints)
    Selection expr name -> do
      fieldType <- freshTypeVar
      (_, exprType, exprAnnotated, constraints) <- infer expr context
      let
        finalConstraints = HasFieldConstraint exprType name : constraints
        annotatedExpression = annotate (selectionWithField exprAnnotated name) fieldType
      pure (context, fieldType, annotatedExpression, finalConstraints)
    Let defs result -> do
      (temporalContext, _, annotatedDefs, varConstraints) <-
        inferDefinitions defs context
      (_, valueType, annotatedValue, valueConstraints) <-
        infer result temporalContext
      let
        annotatedExpression = letExp annotatedDefs annotatedValue
      pure
        ( context
        , valueType
        , annotatedExpression
        , varConstraints
            <> valueConstraints
        )
    Annotation ty expr -> do
      (_, inferredType, annotatedExpression, constraints) <- infer expr context
      pure
        ( context
        , inferredType
        , annotatedExpression
        , EqConstraint ty inferredType : constraints
        )


simplifyConstraint
  :: Constraint
  -> [Constraint]
simplifyConstraint c@(EqConstraint l r) =
  case l of
    TypeVariable _ -> [c]
    Boolean -> [c]
    IntType -> [c]
    Arrow argL outL ->
      case r of
        TypeVariable _ -> [c]
        Boolean -> [c]
        IntType -> [c]
        Arrow argR outR ->
          [EqConstraint argL argR, EqConstraint outL outR]
        RecordType _ -> [c]
    RecordType _ -> [c]
simplifyConstraint c = [c]
