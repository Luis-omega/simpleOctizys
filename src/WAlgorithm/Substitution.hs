module WAlgorithm.Substitution
  ( Substitution
  , SubstitutionItem
  , emptySubstitution
  , singletonSubstitution
  , applySubstitutionItemToType
  , applySubstitutionToConstraint
  , applySubstitutionToType
  , addSubstitutions
  , verifyNotRecursive
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import MyLib
import Prettyprinter (Pretty (pretty), line, (<+>))

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
          throwError (renderDoc errorMsg)
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
      <+> prettyItemList
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
    ( \accType subs@(SubstitutionItem s _) -> do
        pure (applySubstitutionItemToType subs accType)
    )
    ty
    ls


applySubstitutionToItem
  :: Error String :> es
  => Log :> es
  => Substitution
  -> SubstitutionItem
  -> Eff es (SubstitutionItem, Maybe Substitution)
applySubstitutionToItem subs@(Substitution subsLists) (SubstitutionItem name ty) = do
  newType <- applySubstitutionToType subs ty
  let newItem = SubstitutionItem name newType
  case lookup name ((\(SubstitutionItem n t) -> (n, t)) <$> subsLists) of
    Just t -> do
      newSubs <- unify ty t
      pure (newItem, Just newSubs)
    Nothing -> pure (newItem, Nothing)


addSubstitutions
  :: Error String :> es
  => Log :> es
  => Substitution
  -> Substitution
  -> Eff es Substitution
addSubstitutions left@(Substitution itemsL) (Substitution items) = do
  results <- mapM (applySubstitutionToItem left) items
  let
    newItems = fst <$> results
    newSubs = mapMaybe snd results
    newSubstitution = Substitution (itemsL <> newItems)
  foldM addSubstitutions newSubstitution newSubs


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
