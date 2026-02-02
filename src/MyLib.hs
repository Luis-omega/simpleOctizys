{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module MyLib where

import Control.Arrow ((<<<))
import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.State.Static.Local (State, get, put)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , align
  , concatWith
  , defaultLayoutOptions
  , indent
  , layoutPretty
  , line
  , (<+>)
  )
import Prettyprinter.Render.String (renderString)

import Data.Aeson (ToJSON, ToJSONKey)
import GHC.Generics (Generic, Generically (..))


render :: Pretty a => a -> String
render x =
  renderDoc (pretty x)


renderDoc :: Doc ann -> String
renderDoc doc =
  renderString $ layoutPretty defaultLayoutOptions doc


prettyItemList
  :: forall x y ann
   . Pretty x
  => Pretty y
  => [(x, y)]
  -> Doc ann
  -> Doc ann
  -> Doc ann
prettyItemList items sep binder =
  let
    prettyItem :: (x, y) -> Doc ann
    prettyItem (li, ri) =
      pretty li
        <+> binder
        <+> pretty ri
   in
    (align <<< indent 4)
      ( concatWith
          ( \x y ->
              x <> line <> sep <+> y
          )
          (prettyItem <$> items)
      )


newtype Symbol = Symbol
  { unSymbol :: String
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Symbol


instance ToJSONKey Symbol


instance Pretty Symbol where
  pretty s = pretty s.unSymbol


newtype FieldName = FieldName {unFieldName :: String}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically FieldName


instance Pretty FieldName where
  pretty f = pretty f.unFieldName


data SimpleType
  = TypeVariable Symbol
  | Boolean
  | IntType
  | Arrow
      { argument :: SimpleType
      , result :: SimpleType
      }
  | RecordType {typeFields :: Map Symbol SimpleType}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SimpleType


instance Pretty SimpleType where
  pretty (TypeVariable s) = pretty s <> pretty 't'
  pretty Boolean = pretty @String "Bool"
  pretty IntType = pretty @String "Int"
  pretty (Arrow arg res) =
    parens2
      ( pretty arg
          <> pretty @String " -> "
          <> pretty res
      )
  pretty (RecordType fields) =
    let
      items = Map.toList fields
      prettyItems = prettyItemList items (pretty ',') (pretty ':')
     in
      pretty '{'
        <> line
        <> prettyItems
        <> line
        <> pretty '}'


hasTypeVar
  :: Symbol
  -> SimpleType
  -> Bool
hasTypeVar s (TypeVariable sr) = s == sr
hasTypeVar _ Boolean = False
hasTypeVar _ IntType = False
hasTypeVar s (Arrow arg out) =
  hasTypeVar s arg || hasTypeVar s out
hasTypeVar s (RecordType fs) =
  Map.foldr (||) False (Map.map (hasTypeVar s) fs)


data Definition = Definition
  { name :: Symbol
  , annotation :: Maybe SimpleType
  , value :: Expression
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Definition


instance Pretty Definition where
  pretty (Definition name ann value) =
    pretty name
      <> maybe mempty (\x -> pretty ':' <> pretty x) ann
      <> pretty '='
      <> pretty value


data Expression
  = IntLiteral Int
  | BoolLiteral Bool
  | ExpressionVariable Symbol
  | Function {parameter :: Symbol, result :: Expression}
  | Application {function :: Expression, argument :: Expression}
  | Record {fields :: Map Symbol Expression}
  | Selection {receiver :: Expression, fieldName :: FieldName}
  | Let {definitions :: [Definition], result :: Expression}
  | Annotation SimpleType Expression
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Expression


parens2 :: Doc ann -> Doc ann
parens2 x =
  pretty '('
    <> x
    <> pretty ')'


parens :: Pretty a => a -> Doc ann
parens x = parens2 (pretty x)


braces :: Pretty a => a -> Doc ann
braces x =
  pretty '{'
    <> pretty x
    <> pretty '}'


instance Pretty Expression where
  pretty (IntLiteral x) = pretty x
  pretty (BoolLiteral x) = pretty x
  pretty (ExpressionVariable x) = pretty x
  pretty (Function param body) =
    pretty '\\'
      <+> pretty param
      <+> pretty @String "->"
      <+> pretty body
  pretty (Application f arg) =
    parens f <+> parens arg
  pretty (Record fields) =
    let
      items = Map.toList fields
      prettyItems = prettyItemList items (pretty ',') (pretty '=')
     in
      pretty '{'
        <> line
        <> prettyItems
        <> line
        <> pretty '}'
  pretty (Selection receiver name) =
    parens receiver <> pretty '.' <> pretty name
  pretty (Let defs result) =
    pretty @String "let"
      <+> concatWith (\x y -> x <> pretty ';' <> line <> y) (pretty <$> defs)
      <> line
      <> pretty @String "in"
      <> line
      <> pretty result
  pretty (Annotation ty expr) =
    parens2 (parens expr <> pretty ':' <> pretty ty)


newtype Context = Context
  { expressionsMap :: Map Symbol SimpleType
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Context


instance Pretty Context where
  pretty (Context c) =
    let
      items = Map.toList c
      prettyItems =
        prettyItemList items (pretty ',') (pretty ':')
     in
      pretty @String "Context"
        <+> pretty '['
        <> line
        <> prettyItems
        <> line
        <> pretty ']'


emptyContext :: Context
emptyContext = Context mempty


contextFromList
  :: [(String, SimpleType)]
  -> Context
contextFromList ls =
  Context
    ( Map.fromList
        ( first Symbol <$> ls
        )
    )


findExpressionVariable
  :: Error String :> es
  => Symbol
  -> Context
  -> Eff es SimpleType
findExpressionVariable symbol context =
  case Map.lookup symbol context.expressionsMap of
    Just st -> pure st
    Nothing ->
      throwError
        ( "Can't find the required variable "
            <> render symbol
            <> ", context "
            <> render context
        )


addExpressionVars
  :: Error String :> es
  => [(Symbol, SimpleType)]
  -> Context
  -> Eff es Context
addExpressionVars newVars (Context ctx) =
  let
    countUniques =
      Map.fromListWith
        (<>)
        ( (\(x, y) -> (x, [y]))
            <$> newVars
        )
    (regularItems, repeatedItems) =
      Map.foldrWithKey go (Map.empty, Map.empty) countUniques
      where
        go k [x] (m1, m2) = (Map.insert k x m1, m2)
        go k xs (m1, m2) = (m1, Map.insert k xs m2)
   in
    if Map.null regularItems
      then
        let keys = Map.keys regularItems
            lookupResults = (\k -> (\x -> (k, x)) <$> Map.lookup k ctx) <$> keys
         in case catMaybes lookupResults of
              [] -> pure $ Context $ Map.union ctx regularItems
              newValues ->
                let
                  errorMsg :: (Symbol, SimpleType) -> String
                  errorMsg (name, oldValue) =
                    "The variable "
                      <> render name
                      <> " is already defined as "
                      <> render oldValue
                      <> ", and is being redefined as "
                      <> render (Map.lookup name regularItems)
                 in
                  throwError
                    ( "Error:\n"
                        <> intercalate
                          "    \n"
                          ( errorMsg
                              <$> newValues
                          )
                    )
      else
        throwError
          ( "Tried to add duplicate elements to context, duplicates : "
              <> show repeatedItems
          )


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
      <+> parens2 (pretty ty)
      <+> pretty fieldName


freshVariable
  :: State Int :> es
  => Eff es Symbol
freshVariable = do
  currentValue <- get
  put (currentValue + 1)
  pure (Symbol $ show currentValue)


freshTypeVar
  :: State Int :> es
  => Eff es SimpleType
freshTypeVar =
  TypeVariable <$> freshVariable


addTypeToExpression
  :: Context
  -> Symbol
  -> SimpleType
  -> Context
addTypeToExpression context symbol ty =
  Context $ Map.insert symbol ty context.expressionsMap


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
    addExpressionVars
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
      ty <- findExpressionVariable symbol context
      pure (context, ty, [])
    Function param result -> do
      newTypeVar <- freshTypeVar
      (ctx, sty, constraints) <-
        infer result (addTypeToExpression context param newTypeVar)
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
