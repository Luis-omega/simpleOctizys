module WAlgorithm.Substitution
  ( Substitution (Substitution)
  , emptySubstitution
  , singletonSubstitution
  , applySubstitutionToConstraint
  , applySubstitutionToType
  , applySubstitutionToExpression
  , applySubstitutionToParameter
  , applySubstitutionToDefinition
  , verifyNotRecursive
  , addSubstitutions
  ) where

import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Prettyprinter (Pretty (pretty), line, (<+>))

import Ast.Expression
  ( Definition (Definition, annotation, name)
  , Expression (..)
  , Parameter (Parameter)
  , getParameterSymbol
  , getParameterType
  , value
  )
import Ast.Inference (Constraint (..))
import Ast.Symbol (Symbol)
import Ast.Type (SimpleType (..), hasTypeVar)
import Common (throwDocError)
import qualified Common
import Data.Aeson (ToJSON)
import qualified Data.Bifunctor
import Data.Map (Map)
import qualified Data.Maybe
import GHC.Generics (Generic, Generically (..))
import Logging.Effect (Log)
import Logging.Entry (field)
import qualified Logging.Loggers as Log


newtype Substitution = Substitution {unSubstitution :: Map Symbol SimpleType}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Substitution


emptySubstitution :: Substitution
emptySubstitution = Substitution mempty


singletonSubstitution
  :: Symbol -> SimpleType -> Substitution
singletonSubstitution tv ty =
  Substitution (Map.singleton tv ty)


instance Pretty Substitution where
  pretty s =
    pretty '('
      <+> line
      <+> Common.prettyItemList
        ( Data.Bifunctor.first TypeVariable
            <$> Map.toList s.unSubstitution
        )
        (pretty ',')
        (pretty '~')
      <+> line
      <+> pretty ')'


verifyNotRecursive
  :: Error String :> es
  => Log :> es
  => Symbol
  -> SimpleType
  -> Eff es Substitution
verifyNotRecursive s ty =
  if hasTypeVar s ty
    then
      let
        errorMsg =
          pretty @String "Error, variable"
            <+> pretty (TypeVariable s)
            <+> pretty @String "is inside"
            <+> pretty ty
       in
        do
          Log.error
            "Verification of recursivity on type"
            [ field "symbol" (TypeVariable s)
            , field "type" ty
            ]
          throwDocError errorMsg
    else pure (singletonSubstitution s ty)


applySubstitutionToType
  :: Substitution
  -> SimpleType
  -> SimpleType
applySubstitutionToType sub@(Substitution s) ty =
  case ty of
    TypeVariable symbol ->
      Data.Maybe.fromMaybe ty (Map.lookup symbol s)
    Boolean -> ty
    IntType -> ty
    Arrow arg out ->
      let
        newArg = applySubstitutionToType sub arg
        newOut = applySubstitutionToType sub out
       in
        Arrow newArg newOut
    RecordType fs ->
      RecordType $ Map.map (applySubstitutionToType sub) fs


applySubstitutionToConstraint
  :: Substitution
  -> Constraint
  -> Constraint
applySubstitutionToConstraint s (EqConstraint t1 t2) =
  let
    t3 = applySubstitutionToType s t1
    t4 = applySubstitutionToType s t2
   in
    EqConstraint t3 t4
applySubstitutionToConstraint s (HasFieldConstraint ty name) = do
  let newType = applySubstitutionToType s ty
   in HasFieldConstraint newType name


applySubstitutionToParameter
  :: Substitution
  -> Parameter
  -> Parameter
applySubstitutionToParameter s p =
  let
    paramType = applySubstitutionToType s <$> getParameterType p
   in
    Parameter (getParameterSymbol p) paramType


applySubstitutionToDefinition
  :: Substitution
  -> Definition
  -> Definition
applySubstitutionToDefinition s d =
  let
    newAnnotation = applySubstitutionToType s <$> d.annotation
    newExpression = applySubstitutionToExpression s d.value
   in
    Definition d.name newAnnotation newExpression


applySubstitutionToExpression
  :: Substitution
  -> Expression
  -> Expression
applySubstitutionToExpression s expr =
  case expr of
    IntLiteral _ -> expr
    BoolLiteral _ -> expr
    ExpressionVariable _ -> expr
    Function param result ->
      let
        newParam = applySubstitutionToParameter s param
        newResult = applySubstitutionToExpression s result
       in
        Function newParam newResult
    Application f arg ->
      let
        newF = applySubstitutionToExpression s f
        newArg = applySubstitutionToExpression s arg
       in
        Application newF newArg
    Record fs -> Record $ Map.map (applySubstitutionToExpression s) fs
    Selection expre name ->
      let
        newExpr = applySubstitutionToExpression s expre
       in
        Selection newExpr name
    Let defs result ->
      let
        newDefs = applySubstitutionToDefinition s <$> defs
        newResult = applySubstitutionToExpression s result
       in
        Let newDefs newResult
    Annotation ty expre ->
      let
        newTy = applySubstitutionToType s ty
        newExpr = applySubstitutionToExpression s expre
       in
        Annotation newTy newExpr


addSubstitutions
  :: Substitution
  -> Substitution
  -> Substitution
addSubstitutions (Substitution itemsL) (Substitution itemsR) =
  Substitution (Map.union itemsL itemsR)
