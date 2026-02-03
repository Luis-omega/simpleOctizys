module Ast.Expression where

-- \* Pretty imports
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , concatWith
  , line
  , (<+>)
  )
import qualified Prettyprinter as Pretty

-- \* JSON imports
import Data.Aeson (ToJSON)
import GHC.Generics (Generic, Generically (..))

-- \* Internal imports
import Ast.Symbol (FieldName, Symbol)
import Ast.Type (SimpleType)
import qualified Common
import Data.Map (Map)
import qualified Data.Map as Map


data Definition = Definition
  { name :: Symbol
  , annotation :: Maybe SimpleType
  , value :: Expression
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Definition


instance Pretty Definition where
  pretty (Definition name ann value) =
    pretty name
      <> maybe mempty (\x -> pretty ':' <> pretty x) ann
      <> pretty '='
      <> pretty value


data Expression
  = IntLiteral Int
  | BoolLiteral Bool
  | ExpressionVariable Symbol
  | Function {parameter :: Symbol, result :: Expression}
  | Application {function :: Expression, argument :: Expression}
  | Record {fields :: Map Symbol Expression}
  | Selection {receiver :: Expression, fieldName :: FieldName}
  | Let {definitions :: [Definition], result :: Expression}
  | Annotation SimpleType Expression
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Expression


parens :: Pretty a => a -> Doc ann
parens x = Pretty.parens (pretty x)


braces :: Pretty a => a -> Doc ann
braces x = Pretty.braces $ pretty x


instance Pretty Expression where
  pretty (IntLiteral x) = pretty x
  pretty (BoolLiteral x) = pretty x
  pretty (ExpressionVariable x) = pretty x
  pretty (Function param body) =
    pretty '\\'
      <+> pretty param
      <+> pretty @String "->"
      <+> pretty body
  pretty (Application f arg) =
    parens f <+> parens arg
  pretty (Record fields) =
    let
      items = Map.toList fields
      prettyItems = Common.prettyItemList items (pretty ',') (pretty '=')
     in
      pretty '{'
        <> line
        <> prettyItems
        <> line
        <> pretty '}'
  pretty (Selection receiver name) =
    parens receiver <> pretty '.' <> pretty name
  pretty (Let defs result) =
    pretty @String "let"
      <+> concatWith (\x y -> x <> pretty ';' <> line <> y) (pretty <$> defs)
      <> line
      <> pretty @String "in"
      <> line
      <> pretty result
  pretty (Annotation ty expr) =
    Pretty.parens (parens expr <> pretty ':' <> pretty ty)
