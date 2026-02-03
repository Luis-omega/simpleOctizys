module Ast.Combinators where

import Ast.Expression (Definition (..), Expression (..))
import Ast.Symbol (makeField, makeSymbol)
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


function :: String -> Expression -> Expression
function v = Function (makeSymbol v)


application :: Expression -> [Expression] -> Expression
application = foldl Application


record :: [(String, Expression)] -> Expression
record =
  Record
    <<< Map.fromList
    <<< ( Data.Bifunctor.first makeSymbol <$>
        )


selection
  :: Expression -> String -> Expression
selection e field =
  Selection e (makeField field)


letExp :: [Definition] -> Expression -> Expression
letExp = Let


annotate :: Expression -> SimpleType -> Expression
annotate e ty = Annotation ty e


def :: String -> Maybe SimpleType -> Expression -> Definition
def name = Definition (makeSymbol name)
