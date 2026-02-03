module Test.Inference where

import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static
  ( Error
  , runErrorNoCallStack
  )
import Effectful.State.Static.Local (State, runState)
import Effects.Console.Effect (Console)
import Effects.Console.Interpreter (runConsole)
import Logging.Effect (Log)
import Logging.Interpreters.Console (runLog)
import qualified Logging.Levels as Log
import Prettyprinter (line)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion
  , assertEqual
  , assertFailure
  , testCase
  )

import Ast.Combinators
import Ast.Context (Context, contextFromList, emptyContext)
import Ast.Expression (Expression)
import Ast.Type (SimpleType)
import Common (prettyWithHeader, renderDoc)
import qualified Data.Bifunctor as Bifunctor
import WAlgorithm.Unification (solveExpressionFullInfo)


tests :: TestTree
tests =
  testGroup
    ""
    [ testCase "Literal int" $ do
        assertInfers emptyContext (int 1) intType
    , testCase "Literal bool" $
        assertInfers emptyContext (bool True) boolType
    , testCase "Identity function" $
        assertInfers
          emptyContext
          (function (parameter "x" (Just intType)) (var "x"))
          (arrow [intType] intType)
    , testCase "Partial application" $
        assertInfers
          (contextFromList [add])
          ( application
              (var "add")
              [int 1]
          )
          (arrow [intType] intType)
    , testCase "Let binding" $
        assertInfers
          emptyContext
          ( letExp
              [def "x" (Just intType) (int 1)]
              (var "x")
          )
          intType
    , testCase "Let-bound function" $
        assertInfers
          emptyContext
          ( letExp
              [ def
                  "id"
                  (Just (arrow [intType] intType))
                  ( function
                      (parameter "x" (Just intType))
                      (var "x")
                  )
              ]
              (application (var "id") [int 1])
          )
          intType
    , testCase "Record literal" $
        assertInfers
          emptyContext
          ( record
              [ ("l1", int 1)
              , ("l2", bool True)
              ]
          )
          ( recordType
              [ ("l1", intType)
              , ("l2", boolType)
              ]
          )
    , testCase "Record field selection" $
        assertInfers
          emptyContext
          ( selection
              ( record
                  [ ("l1", int 1)
                  , ("l2", bool True)
                  ]
              )
              "l1"
          )
          intType
    , testCase "Function accessing record field" $
        assertInfers
          emptyContext
          ( function
              (parameter "r" (Just (recordType [("x", intType)])))
              (selection (var "r") "x")
          )
          ( arrow
              [recordType [("x", intType)]]
              intType
          )
    , testCase "Type error in application" $ do
        assertFails
          (contextFromList [add])
          ( application
              (var "add")
              [ bool True
              , int 1
              ]
          )
    , testCase "Selecting missing field fails" $ do
        assertFails
          emptyContext
          ( selection
              (record [("x", int 1)])
              "y"
          )
    ]


inferenceWithEffects
  :: Error String :> es
  => Log :> es
  => Console :> es
  => State Int :> es
  => Context
  -> Expression
  -> Eff es SimpleType
inferenceWithEffects context expression = do
  ( _
    , _
    , _
    , _
    , solvedType
    , _
    ) <-
    solveExpressionFullInfo
      context
      expression
  pure solvedType


inferenceIO :: Context -> Expression -> IO (Either String SimpleType)
inferenceIO context expression = do
  maybeResult <-
    runEff
      ( runConsole
          ( runLog
              Log.Error
              ( runErrorNoCallStack
                  ( runState
                      0
                      ( inferenceWithEffects
                          context
                          expression
                      )
                  )
              )
          )
      )
  pure $ Bifunctor.second fst maybeResult


showInformation :: Context -> Expression -> String
showInformation context expression =
  let finalDoc =
        prettyItem "Context" context
          <> prettyItem "Original expression" expression
   in renderDoc finalDoc
  where
    prettyItem name doc =
      prettyWithHeader name doc
        <> line


add :: (String, SimpleType)
add = ("add", arrow [intType, intType] intType)


ifFunction :: (String, SimpleType)
ifFunction = ("if", arrow [boolType, intType, intType] intType)


assertInfers :: Context -> Expression -> SimpleType -> Assertion
assertInfers context expression expected = do
  result :: Either String SimpleType <- inferenceIO context expression
  assertEqual
    (showInformation context expression)
    (pure expected)
    result


assertFails
  :: Context
  -> Expression
  -> Assertion
assertFails context expr = do
  result <- inferenceIO context expr
  case result of
    Left _ -> pure ()
    Right t ->
      assertFailure $
        "Expected type error, but inferred: "
          <> show t
