module Ast.Combinators where

import Ast.Expression
  ( Definition (..)
  , Expression
    ( Annotation
    , Application
    , BoolLiteral
    , ExpressionVariable
    , Function
    , IntLiteral
    , Let
    , Record
    , Selection
    )
  , Parameter (Parameter)
  )
import Ast.Symbol (FieldName, Symbol, makeField, makeSymbol)
import Ast.Type (SimpleType (..))
import Control.Arrow ((<<<))
import qualified Data.Bifunctor
import qualified Data.Map as Map


typeVar :: String -> SimpleType
typeVar x = TypeVariable (makeSymbol x)


boolType :: SimpleType
boolType = Boolean


intType :: SimpleType
intType = IntType


arrow :: [SimpleType] -> SimpleType -> SimpleType
arrow xs ty = foldr Arrow ty xs


recordType :: [(String, SimpleType)] -> SimpleType
recordType =
  RecordType
    <<< Map.fromList
    <<< ( Data.Bifunctor.first makeSymbol <$>
        )


int :: Int -> Expression
int = IntLiteral


bool :: Bool -> Expression
bool = BoolLiteral


var :: String -> Expression
var x = ExpressionVariable (makeSymbol x)


parameter :: String -> Maybe SimpleType -> Parameter
parameter v = parameterWithSymbol (makeSymbol v)


parameterWithSymbol :: Symbol -> Maybe SimpleType -> Parameter
parameterWithSymbol = Parameter


function :: Parameter -> Expression -> Expression
function = Function


application :: Expression -> [Expression] -> Expression
application = foldl Application


record :: [(String, Expression)] -> Expression
record =
  Record
    <<< Map.fromList
    <<< ( Data.Bifunctor.first makeSymbol <$>
        )


recordWithSymbols :: [(Symbol, Expression)] -> Expression
recordWithSymbols =
  Record
    <<< Map.fromList


selection
  :: Expression -> String -> Expression
selection e field =
  Selection e (makeField field)


selectionWithField :: Expression -> FieldName -> Expression
selectionWithField = Selection


letExp :: [Definition] -> Expression -> Expression
letExp = Let


annotate :: Expression -> SimpleType -> Expression
annotate e ty = Annotation ty e


def :: String -> Maybe SimpleType -> Expression -> Definition
def name = Definition (makeSymbol name)
