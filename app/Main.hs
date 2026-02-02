module Main where

import Combinators
import qualified Data.Text as Text
import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.State.Static.Local (State, runState)
import Effects.Console.Effect (Console)
import qualified Effects.Console.Effect as Console
import Effects.Console.Interpreter (runConsole)
import Logging.Effect (Log)
import Logging.Interpreters.Console (runLog)
import qualified Logging.Levels as Log
import MyLib
  ( Context
  , Expression
  , contextFromList
  , renderDoc
  , simplifyConstraint
  )
import Prettyprinter (indent, line, pretty)
import WAlgorithm
  ( solveExpressionFullInfo
  )


testContext :: Context
testContext =
  contextFromList
    [ ("add", arrow [intType, intType] intType)
    , ("if", arrow [boolType, intType, intType] intType)
    ]


goodIf :: Expression
goodIf =
  application
    (var "if")
    [ bool True
    , int 2
    , int 0
    ]


infiniteRecursion :: Expression
infiniteRecursion =
  function "x" $
    application
      (var "x")
      [ var "x"
      ]


testExpression :: Expression
testExpression = infiniteRecursion


mainEffect
  :: Error String :> es
  => Log :> es
  => Console :> es
  => State Int :> es
  => Eff es ()
mainEffect = do
  (context, inferredType, constraints, substitution, solvedType) <-
    solveExpressionFullInfo
      testContext
      testExpression
  let
    simplifiedConstraints = concatMap simplifyConstraint constraints
    simplifiedConstraintsDoc = pretty simplifiedConstraints
    ctxDoc = pretty context
    expressionDoc = pretty testExpression
    inferredTypeDoc = pretty inferredType
    constraintsDoc = pretty constraints
    substitutionDoc = pretty substitution
    solvedTypeDoc = pretty solvedType
    finalDoc =
      prettyItem "Context" ctxDoc
        <> prettyItem "Original expression" expressionDoc
        <> prettyItem "Inferred type" inferredTypeDoc
        <> prettyItem "Generated Constraints" constraintsDoc
        <> prettyItem "Simplied constraints" simplifiedConstraintsDoc
        <> prettyItem "Substitution" substitutionDoc
        <> prettyItem "Solved Type" solvedTypeDoc
  Console.putText (Text.pack (renderDoc finalDoc))
  where
    prettyItem name doc =
      pretty @String name
        <> line
        <> indent 4 doc
        <> line


main :: IO ()
main = do
  maybeResult <-
    runEff
      ( runConsole
          ( runLog
              Log.Trace
              ( runErrorNoCallStack
                  ( runState
                      0
                      mainEffect
                  )
              )
          )
      )
  case maybeResult of
    Left msg -> putStrLn msg
    Right _ -> pure ()
