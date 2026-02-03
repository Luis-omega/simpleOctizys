module Ast.Type where

import Data.Map (Map)
import qualified Data.Map as Map

-- \* Pretty imports
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , line
  )
import qualified Prettyprinter as Pretty

-- \* JSON imports
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

-- \* Internal imports
import Ast.Symbol (Symbol)
import qualified Common


data SimpleType
  = TypeVariable Symbol
  | Boolean
  | IntType
  | Arrow
      { argument :: SimpleType
      , result :: SimpleType
      }
  | RecordType {typeFields :: Map Symbol SimpleType}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically SimpleType


prettyTypeMaybeParens :: SimpleType -> Doc ann
prettyTypeMaybeParens t@(TypeVariable _) = pretty t
prettyTypeMaybeParens t@Boolean = pretty t
prettyTypeMaybeParens t@IntType = pretty t
prettyTypeMaybeParens t@(Arrow _ _) = Pretty.parens (pretty t)
prettyTypeMaybeParens t@(RecordType _) = pretty t


instance Pretty SimpleType where
  pretty (TypeVariable s) = pretty s <> pretty 't'
  pretty Boolean = pretty @String "Bool"
  pretty IntType = pretty @String "Int"
  pretty (Arrow arg res) =
    prettyTypeMaybeParens arg
      <> pretty @String " -> "
      <> pretty res
  pretty (RecordType fields) =
    let
      items = Map.toList fields
      prettyItems = Common.prettyItemList items (pretty ',') (pretty ':')
     in
      pretty '{'
        <> line
        <> prettyItems
        <> line
        <> pretty '}'


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
