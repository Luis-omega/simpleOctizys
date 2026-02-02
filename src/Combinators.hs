module Combinators where

import Control.Arrow ((<<<))
import qualified Data.Bifunctor
import qualified Data.Map as Map
import MyLib


typeVar :: String -> SimpleType
typeVar x = TypeVariable (Symbol x)


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
    <<< ( Data.Bifunctor.first Symbol <$>
        )


int :: Int -> Expression
int = IntLiteral


bool :: Bool -> Expression
bool = BoolLiteral


var :: String -> Expression
var x = ExpressionVariable (Symbol x)


function :: String -> Expression -> Expression
function v = Function (Symbol v)


application :: Expression -> [Expression] -> Expression
application = foldl Application


record :: [(String, Expression)] -> Expression
record =
  Record
    <<< Map.fromList
    <<< ( Data.Bifunctor.first Symbol <$>
        )


selection
  :: Expression -> String -> Expression
selection e field =
  Selection e (FieldName field)


letExp :: [Definition] -> Expression -> Expression
letExp = Let


annotate :: Expression -> SimpleType -> Expression
annotate e ty = Annotation ty e


def :: String -> Maybe SimpleType -> Expression -> Definition
def name = Definition (Symbol name)
