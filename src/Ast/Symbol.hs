module Ast.Symbol (Symbol, FieldName, makeSymbol, makeField) where

import Prettyprinter
  ( Pretty (pretty)
  )

-- \* JSON imports
import Data.Aeson (ToJSON, ToJSONKey)
import GHC.Generics (Generic, Generically (..))


-- | The underliying representation of a symbol, right now is just a String.
newtype Symbol = Symbol
  { unSymbol :: String
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Symbol


instance ToJSONKey Symbol


instance Pretty Symbol where
  pretty s = pretty s.unSymbol


makeSymbol :: String -> Symbol
makeSymbol = Symbol


newtype FieldName = FieldName {unFieldName :: String}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically FieldName


instance Pretty FieldName where
  pretty f = pretty f.unFieldName


makeField :: String -> FieldName
makeField = FieldName
