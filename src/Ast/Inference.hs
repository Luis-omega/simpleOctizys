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
import qualified Prettyprinter as Pretty

-- \* JSON imports
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

-- \* Internal imports

import Ast.Context (Context)
import qualified Ast.Context as Context
import Ast.Expression (Definition (..), Expression (..))
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
      <+> Pretty.parens (pretty ty)
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
  -> Eff es (Context, [(Symbol, SimpleType)], [Constraint])
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
            (_, inferredType, generatedConstraints) <- infer value augmentedContext
            let
              newConstraint = EqConstraint assignedType inferredType
              newConstraints = newConstraint : generatedConstraints
            pure ((name, inferredType), newConstraints)
      )
      variablesTypes
  let
    newConstraints = annotationConstraints <> concatMap snd results
    typedSymbols = fst <$> results
  pure (augmentedContext, typedSymbols, newConstraints)


infer
  :: Error String :> es
  => State Int :> es
  => Expression
  -> Context
  -> Eff es (Context, SimpleType, [Constraint])
infer expression context =
  case expression of
    IntLiteral _ -> pure (context, IntType, [])
    BoolLiteral _ -> pure (context, Boolean, [])
    ExpressionVariable symbol -> do
      ty <- Context.findExpressionVariable symbol context
      pure (context, ty, [])
    Function param result -> do
      newTypeVar <- freshTypeVar
      (ctx, sty, constraints) <-
        infer result (Context.addTypeToExpression context param newTypeVar)
      pure
        ( ctx
        , Arrow
            newTypeVar
            sty
        , constraints
        )
    Application f arg -> do
      resultType <- freshTypeVar
      (_, fType, constraintsF) <- infer f context
      (_, argType, constraintsArg) <- infer arg context
      let newArrow = Arrow argType resultType
          newConstraints =
            EqConstraint fType newArrow
              : ( constraintsF
                    <> constraintsArg
                )
      pure (context, resultType, newConstraints)
    Record fs -> do
      items <-
        mapM
          ( \(name, x) -> do
              (_, ty, css) <- infer x context
              pure ((name, ty), css)
          )
          (Map.toList fs)
      let
        fieldTypes = fst <$> items
        constraints = snd <$> items
        recordTy = RecordType (Map.fromList fieldTypes)
      pure (context, recordTy, foldl (<>) [] constraints)
    Selection expr name -> do
      fieldType <- freshTypeVar
      (_, exprType, constraints) <- infer expr context
      let
        finalConstraints = HasFieldConstraint exprType name : constraints
      pure (context, fieldType, finalConstraints)
    Let defs result -> do
      (temporalContext, _, varConstraints) <- inferDefinitions defs context
      (_, valueType, valueConstraints) <- infer result temporalContext
      pure
        ( context
        , valueType
        , varConstraints
            <> valueConstraints
        )
    Annotation ty expr -> do
      (_, inferredType, constraints) <- infer expr context
      pure
        ( context
        , inferredType
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
