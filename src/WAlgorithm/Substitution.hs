module WAlgorithm.Substitution
  ( Substitution (Substitution)
  , SubstitutionItem (SubstitutionItem)
  , emptySubstitution
  , singletonSubstitution
  , applySubstitutionItemToType
  , applySubstitutionToConstraint
  , applySubstitutionToType
  , applySubstitutionToExpression
  , applySubstitutionToParameter
  , applySubstitutionToDefinition
  , verifyNotRecursive
  ) where

import Control.Monad (foldM)
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
import GHC.Generics (Generic, Generically (..))
import Logging.Effect (Log)
import Logging.Entry (field)
import qualified Logging.Loggers as Log


data SubstitutionItem = SubstitutionItem Symbol SimpleType
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SubstitutionItem


newtype Substitution
  = Substitution [SubstitutionItem]
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Substitution


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
    else pure (Substitution [SubstitutionItem s ty])


emptySubstitution :: Substitution
emptySubstitution = Substitution []


singletonSubstitution
  :: Symbol -> SimpleType -> Substitution
singletonSubstitution tv ty =
  Substitution [SubstitutionItem tv ty]


instance Pretty SubstitutionItem where
  pretty (SubstitutionItem s ty) =
    pretty (TypeVariable s)
      <+> pretty '~'
      <+> pretty ty


instance Pretty Substitution where
  pretty (Substitution ls) =
    pretty '('
      <+> line
      <+> Common.prettyItemList
        ((\(SubstitutionItem s ty) -> (TypeVariable s, ty)) <$> ls)
        (pretty ',')
        (pretty '~')
      <+> line
      <+> pretty ')'


applySubstitutionItemToType
  :: SubstitutionItem
  -> SimpleType
  -> SimpleType
applySubstitutionItemToType subs@(SubstitutionItem symbol value) ty =
  case ty of
    TypeVariable s ->
      if s == symbol
        then value
        else ty
    Boolean -> ty
    IntType -> ty
    Arrow arg out ->
      Arrow
        (applySubstitutionItemToType subs arg)
        (applySubstitutionItemToType subs out)
    RecordType fs ->
      RecordType $
        Map.map (applySubstitutionItemToType subs) fs


applySubstitutionToType
  :: Error String :> es
  => Substitution
  -> SimpleType
  -> Eff es SimpleType
applySubstitutionToType (Substitution ls) ty =
  foldM
    ( \accType subs -> do
        pure (applySubstitutionItemToType subs accType)
    )
    ty
    ls


applySubstitutionToConstraint
  :: Error String :> es
  => Log :> es
  => Substitution
  -> Constraint
  -> Eff es Constraint
applySubstitutionToConstraint s (EqConstraint t1 t2) = do
  t3 <- applySubstitutionToType s t1
  t4 <- applySubstitutionToType s t2
  pure $ EqConstraint t3 t4
applySubstitutionToConstraint s (HasFieldConstraint ty name) = do
  newType <- applySubstitutionToType s ty
  pure $ HasFieldConstraint newType name


applySubstitutionToParameter
  :: Error String :> es
  => Log :> es
  => Substitution
  -> Parameter
  -> Eff es Parameter
applySubstitutionToParameter s p = do
  paramType <- mapM (applySubstitutionToType s) (getParameterType p)
  pure $ Parameter (getParameterSymbol p) paramType


applySubstitutionToDefinition
  :: Error String :> es
  => Log :> es
  => Substitution
  -> Definition
  -> Eff es Definition
applySubstitutionToDefinition s d = do
  newAnnotation <- mapM (applySubstitutionToType s) d.annotation
  newExpression <- applySubstitutionToExpression s d.value
  pure $ Definition d.name newAnnotation newExpression


applySubstitutionToExpression
  :: Error String :> es
  => Log :> es
  => Substitution
  -> Expression
  -> Eff es Expression
applySubstitutionToExpression s expr =
  case expr of
    IntLiteral _ -> pure expr
    BoolLiteral _ -> pure expr
    ExpressionVariable _ -> pure expr
    Function param result -> do
      newParam <- applySubstitutionToParameter s param
      newResult <- applySubstitutionToExpression s result
      pure $ Function newParam newResult
    Application f arg -> do
      newF <- applySubstitutionToExpression s f
      newArg <- applySubstitutionToExpression s arg
      pure $ Application newF newArg
    Record fs -> do
      let
        items = Map.toList fs
      newItems <-
        mapM
          ( \(x, y) -> do
              yResult <- applySubstitutionToExpression s y
              pure (x, yResult)
          )
          items
      pure $ Record $ Map.fromList newItems
    Selection expre name -> do
      newExpr <- applySubstitutionToExpression s expre
      pure $ Selection newExpr name
    Let defs result -> do
      newDefs <- mapM (applySubstitutionToDefinition s) defs
      newResult <- applySubstitutionToExpression s result
      pure $ Let newDefs newResult
    Annotation ty expre -> do
      newTy <- applySubstitutionToType s ty
      newExpr <- applySubstitutionToExpression s expre
      pure $ Annotation newTy newExpr
