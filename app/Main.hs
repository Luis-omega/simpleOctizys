module Main where

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
import Prettyprinter (line)

-- \* Internal imports

import Ast.Combinators
import Ast.Context (Context, contextFromList)
import Ast.Expression (Expression)
import Ast.Inference (simplifyConstraint)
import Common (prettyWithHeader, renderDoc)
import WAlgorithm.Unification (solveExpressionFullInfo)


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
  function (parameter "x" Nothing) $
    application
      (var "x")
      [ var "x"
      ]


testExpression :: Expression
testExpression = goodIf


mainEffect
  :: Error String :> es
  => Log :> es
  => Console :> es
  => State Int :> es
  => Eff es ()
mainEffect = do
  ( context
    , inferredType
    , constraints
    , substitution
    , solvedType
    , annotatedExpression
    ) <-
    solveExpressionFullInfo
      testContext
      testExpression
  let
    simplifiedConstraints = concatMap simplifyConstraint constraints
    finalDoc =
      prettyItem "Context" context
        <> prettyItem "Original expression" annotatedExpression
        <> prettyItem "Inferred type" inferredType
        <> prettyItem "Generated Constraints" constraints
        <> prettyItem "Simplied constraints" simplifiedConstraints
        <> prettyItem "Substitution" substitution
        <> prettyItem "Solved Type" solvedType
  Console.putText (Text.pack (renderDoc finalDoc))
  where
    prettyItem name doc =
      prettyWithHeader name doc
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
