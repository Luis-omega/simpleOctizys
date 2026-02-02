module WAlgorithm.Unification where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError, tryError)
import Effectful.State.Static.Local (State)
import MyLib
import Prettyprinter (Pretty (pretty), indent, line, (<+>))

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
        "After adding susbtitution to decomposed constraints"
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
              <> pretty @String "Inferred type:"
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
                  <> pretty @String "Inferred type:"
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
