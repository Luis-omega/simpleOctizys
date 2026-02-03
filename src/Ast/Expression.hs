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


data Parameter = Parameter
  { name :: Symbol
  , annotation :: Maybe SimpleType
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via Generically Parameter


instance Pretty Parameter where
  pretty (Parameter n (Just an)) = Pretty.parens (pretty n <+> pretty ':' <+> pretty an)
  pretty (Parameter n Nothing) = pretty n


getParameterSymbol :: Parameter -> Symbol
getParameterSymbol (Parameter n _) = n


getParameterType :: Parameter -> Maybe SimpleType
getParameterType (Parameter _ an) = an


data Expression
  = IntLiteral Int
  | BoolLiteral Bool
  | ExpressionVariable Symbol
  | Function {parameter :: Parameter, result :: Expression}
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


prettyExpressionMaybeParens :: Expression -> Doc ann
prettyExpressionMaybeParens t@(IntLiteral _) = pretty t
prettyExpressionMaybeParens t@(BoolLiteral _) = pretty t
prettyExpressionMaybeParens t@(ExpressionVariable _) = pretty t
prettyExpressionMaybeParens t@(Function _ _) = Pretty.parens $ pretty t
prettyExpressionMaybeParens t@(Application _ _) = Pretty.parens $ pretty t
prettyExpressionMaybeParens t@(Record _) = pretty t
prettyExpressionMaybeParens t@(Selection _ _) = pretty t
prettyExpressionMaybeParens t@(Let _ _) = Pretty.parens $ pretty t
prettyExpressionMaybeParens t@(Annotation _ _) = Pretty.parens $ pretty t


prettyFuncMaybeParens :: Expression -> Doc ann
prettyFuncMaybeParens t@(IntLiteral _) = pretty t
prettyFuncMaybeParens t@(BoolLiteral _) = pretty t
prettyFuncMaybeParens t@(ExpressionVariable _) = pretty t
prettyFuncMaybeParens t@(Function _ _) = Pretty.parens $ pretty t
prettyFuncMaybeParens t@(Application _ _) = pretty t
prettyFuncMaybeParens t@(Record _) = pretty t
prettyFuncMaybeParens t@(Selection _ _) = pretty t
prettyFuncMaybeParens t@(Let _ _) = Pretty.parens $ pretty t
prettyFuncMaybeParens t@(Annotation _ _) = Pretty.parens $ pretty t


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
    prettyFuncMaybeParens f <+> prettyExpressionMaybeParens arg
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
    prettyExpressionMaybeParens receiver <> pretty '.' <> pretty name
  pretty (Let defs result) =
    pretty @String "let"
      <+> concatWith (\x y -> x <> pretty ';' <> line <> y) (pretty <$> defs)
      <> line
      <> pretty @String "in"
      <> line
      <> pretty result
  pretty (Annotation ty expr) =
    prettyExpressionMaybeParens expr <> pretty ':' <> pretty ty
