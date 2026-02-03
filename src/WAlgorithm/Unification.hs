module WAlgorithm.Unification where

import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError, tryError)
import Effectful.State.Static.Local (State)
import Prettyprinter (Pretty (pretty), hardline, (<+>))

import Ast.Context (Context)
import Ast.Expression (Expression)
import Ast.Inference (Constraint (..), infer)
import Ast.Symbol (Symbol)
import Ast.Type (SimpleType (..))
import Common (prettyWithHeader, throwDocError)
import Logging.Effect (Log)
import Logging.Entry (field)
import qualified Logging.Loggers as Log
import WAlgorithm.Substitution


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
          throwDocError
            ( prefixMessage
                <+> pretty @String "a Bool with"
                <+> pretty rt
            )
    IntType ->
      case rt of
        IntType ->
          pure emptySubstitution
        TypeVariable rs ->
          pure (singletonSubstitution rs IntType)
        _ ->
          throwDocError
            ( prefixMessage
                <+> pretty @String "a Int with"
                <+> pretty rt
            )
    TypeVariable ls ->
      verifyNotRecursive ls rt
    Arrow argl outl ->
      case rt of
        TypeVariable sr -> verifyNotRecursive sr lt
        Arrow argr outr -> do
          argSubs <- unify argl argr
          let
            outlSubs = applySubstitutionToType argSubs outl
            outRSubs = applySubstitutionToType argSubs outr
          outSubs <- unify outlSubs outRSubs
          pure $ addSubstitutions argSubs outSubs
        _ ->
          throwDocError
            ( prefixMessage
                <+> pretty lt
                <+> pretty @String "with"
                <+> pretty rt
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
                  pure $ foldr addSubstitutions emptySubstitution subs
                ([], _) ->
                  throwDocError
                    ( prefixMessage
                        <+> pretty lt
                        <+> pretty @String "with"
                        <+> pretty rt
                        <+> "fields in the right record that aren't on the left one are"
                        <+> pretty onlyR
                    )
                (_, []) ->
                  throwDocError
                    ( prefixMessage
                        <+> pretty lt
                        <+> pretty @String "with"
                        <+> pretty rt
                        <+> "fields in the left record that aren't on the right one are"
                        <+> pretty onlyL
                    )
                _ ->
                  throwDocError
                    ( prefixMessage
                        <+> pretty lt
                        <+> pretty @String "with"
                        <+> pretty rt
                        <+> "fields in the right record that aren't on the left one are"
                        <+> pretty onlyR
                        <+> "fields in the left record that aren't on the right one are"
                        <+> pretty onlyL
                    )
        _ ->
          throwDocError
            ( prefixMessage
                <+> pretty lt
                <+> pretty @String "with"
                <+> pretty rt
            )
  where
    prefixMessage = pretty @String "Can't unify"


solveConstraint
  :: Error String :> es
  => Log :> es
  => Constraint
  -> Eff es Substitution
solveConstraint (EqConstraint tl tr) =
  unify tl tr
solveConstraint (HasFieldConstraint _ _) =
  throwError "Not implemented has field constraint resolution"


solveConstraints
  :: Error String :> es
  => Log :> es
  => [Constraint]
  -> Eff es Substitution
solveConstraints constraints =
  go constraints emptySubstitution
  where
    go [] acc = pure acc
    go (constr : remain) acc = do
      newSubs <- solveConstraint constr
      Log.trace
        "After decompossing"
        [ field "original constr" constr
        , field "new substitutions" newSubs
        ]
      let
        finalSub = addSubstitutions acc newSubs
      Log.trace
        "After adding susbtitution to decomposed constraints"
        [ field "original constr" constr
        , field "new substitutions" newSubs
        , field "merged substitution" finalSub
        ]
      let
        newConstraints = applySubstitutionToConstraint finalSub <$> remain
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
  -> Eff
      es
      (SimpleType, [Constraint], Substitution, SimpleType, Expression)
solveExpressionFullInfo context expression = do
  (inferType, annotatedExpression, constraints) <-
    infer expression context
  maybeSubstitution <- tryError (solveConstraints constraints)
  case maybeSubstitution of
    Left (_, e) ->
      throwDocError
        ( prettyWithHeader "Expression" annotatedExpression
            <> hardline
            <> prettyWithHeader "Inferred type" inferType
            <> hardline
            <> prettyWithHeader "Constraints" constraints
            <> hardline
            <> pretty e
        )
    Right substitution ->
      let
        solvedType = applySubstitutionToType substitution inferType
       in
        pure
          ( inferType
          , constraints
          , substitution
          , solvedType
          , annotatedExpression
          )


solveExpression
  :: Error String :> es
  => Log :> es
  => State Int :> es
  => Context
  -> Expression
  -> Eff es (SimpleType, Expression)
solveExpression context expression = do
  (_, _, _, finalType, finalExpression) <-
    solveExpressionFullInfo context expression
  pure (finalType, finalExpression)
