module WAlgorithm where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError, tryError)
import Effectful.State.Static.Local (State)
import MyLib
import Prettyprinter (Pretty (pretty), indent, line, (<+>))

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


compareRecords
  :: Map Symbol SimpleType
  -> Map Symbol SimpleType
  -> ( [(Symbol, SimpleType, SimpleType)] -- common fields
     , [(Symbol, SimpleType)] -- only on r1
     , [(Symbol, SimpleType)] -- only on r2
     )
compareRecords r1 r2 =
  ( both
  , only1
  , only2
  )
  where
    both =
      [ (k, t1, t2)
      | (k, t1) <- Map.toList r1
      , Just t2 <- [Map.lookup k r2]
      ]

    only1 =
      Map.toList (r1 `Map.difference` r2)

    only2 =
      Map.toList (r2 `Map.difference` r1)


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


unify
  :: Error String :> es
  => Log :> es
  => SimpleType
  -> SimpleType
  -> Eff es Substitution
unify lt rt =
  case lt of
    Boolean ->
      case rt of
        Boolean ->
          pure emptySubstitution
        TypeVariable rs ->
          pure (singletonSubstitution rs Boolean)
        _ ->
          throwError
            ( renderDoc
                ( prefixMessage
                    <+> pretty @String "a Bool with"
                    <+> pretty rt
                )
            )
    IntType ->
      case rt of
        IntType ->
          pure emptySubstitution
        TypeVariable rs ->
          pure (singletonSubstitution rs IntType)
        _ ->
          throwError
            ( renderDoc
                ( prefixMessage
                    <+> pretty @String "a Int with"
                    <+> pretty rt
                )
            )
    TypeVariable ls ->
      verifyNotRecursive ls rt
    Arrow argl outl ->
      case rt of
        TypeVariable sr -> verifyNotRecursive sr lt
        Arrow argr outr -> do
          argSubs <- unify argl argr
          outlSubs <- applySubstitutionToType argSubs outl
          outRSubs <- applySubstitutionToType argSubs outr
          outSubs <- unify outlSubs outRSubs
          addSubstitutions argSubs outSubs
        _ ->
          throwError
            ( renderDoc
                ( prefixMessage
                    <+> pretty lt
                    <+> pretty @String "with"
                    <+> pretty rt
                )
            )
    RecordType fsl ->
      case rt of
        TypeVariable sr -> verifyNotRecursive sr lt
        RecordType fsr ->
          let
            (common, onlyL, onlyR) = compareRecords fsl fsr
           in
            do
              subs <-
                traverse (\(_, t1, t2) -> unify t1 t2) common
              case (onlyL, onlyR) of
                ([], []) ->
                  foldM addSubstitutions emptySubstitution subs
                ([], _) ->
                  throwError
                    ( renderDoc
                        ( prefixMessage
                            <+> pretty lt
                            <+> pretty @String "with"
                            <+> pretty rt
                            <+> "fields in the right record that aren't on the left one are"
                            <+> pretty onlyR
                        )
                    )
                (_, []) ->
                  throwError
                    ( renderDoc
                        ( prefixMessage
                            <+> pretty lt
                            <+> pretty @String "with"
                            <+> pretty rt
                            <+> "fields in the left record that aren't on the right one are"
                            <+> pretty onlyL
                        )
                    )
                _ ->
                  throwError
                    ( renderDoc
                        ( prefixMessage
                            <+> pretty lt
                            <+> pretty @String "with"
                            <+> pretty rt
                            <+> "fields in the right record that aren't on the left one are"
                            <+> pretty onlyR
                            <+> "fields in the left record that aren't on the right one are"
                            <+> pretty onlyL
                        )
                    )
        _ ->
          throwError
            ( renderDoc
                ( prefixMessage
                    <+> pretty lt
                    <+> pretty @String "with"
                    <+> pretty rt
                )
            )
  where
    prefixMessage = pretty @String "Can't unify"


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


decomposeConstraint
  :: Error String :> es
  => Log :> es
  => Constraint
  -> Eff es Substitution
decomposeConstraint (EqConstraint tl tr) =
  unify tl tr
decomposeConstraint (HasFieldConstraint _ _) =
  throwError "Not implemented has field constraint resolution"


decomposeConstraints
  :: Error String :> es
  => Log :> es
  => [Constraint]
  -> Eff es Substitution
decomposeConstraints constraints =
  go constraints emptySubstitution
  where
    go [] acc = pure acc
    go (constr : remain) acc = do
      newSubs <- decomposeConstraint constr
      Log.trace
        "After decompossing"
        [ field "original constr" constr
        , field "new substitutions" newSubs
        ]
      finalSub <- addSubstitutions acc newSubs
      Log.trace
        "After adding subtitution to decomposed constraints"
        [ field "original constr" constr
        , field "new substitutions" newSubs
        , field "merged substitution" finalSub
        ]
      newConstraints <- mapM (applySubstitutionToConstraint finalSub) remain
      Log.trace
        "After new substitution is applied to all remain constraints"
        [ field "original constr" constr
        , field "new substitutions" newSubs
        , field "merged substitution" finalSub
        , field "original constraints" remain
        , field "substituted constraints" newConstraints
        ]
      go newConstraints finalSub


solveExpressionFullInfo
  :: Error String :> es
  => Log :> es
  => State Int :> es
  => Context
  -> Expression
  -> Eff es (Context, SimpleType, [Constraint], Substitution, SimpleType)
solveExpressionFullInfo context expression = do
  (finalContext, inferType, constraints) <- infer expression context
  maybeSubstitution <- tryError (decomposeConstraints constraints)
  case maybeSubstitution of
    Left (_, e) ->
      throwError $
        renderDoc
          ( pretty @String "Expression:"
              <> line
              <> indent 4 (pretty expression)
              <> line
              <> pretty @String "Infered type:"
              <> line
              <> indent 4 (pretty inferType)
              <> line
              <> pretty @String "Constraints:"
              <> line
              <> indent 4 (pretty constraints)
              <> line
              <> pretty e
          )
    Right substitution -> do
      maybeSolvedType <- tryError (applySubstitutionToType substitution inferType)
      case maybeSolvedType of
        Left (_, e) ->
          throwError $
            renderDoc
              ( pretty @String "Expression:"
                  <> line
                  <> indent 4 (pretty expression)
                  <> line
                  <> pretty @String "Infered type:"
                  <> line
                  <> indent 4 (pretty inferType)
                  <> line
                  <> pretty @String "Constraints:"
                  <> line
                  <> indent 4 (pretty constraints)
                  <> line
                  <> pretty e
              )
        Right solvedType ->
          pure (finalContext, inferType, constraints, substitution, solvedType)


solveExpression
  :: Error String :> es
  => Log :> es
  => State Int :> es
  => Context
  -> Expression
  -> Eff es SimpleType
solveExpression context expression = do
  (_, _, _, _, finalType) <- solveExpressionFullInfo context expression
  pure finalType
