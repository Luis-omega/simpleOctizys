module Common where

import Control.Arrow ((<<<))
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Prettyprinter
  ( Doc
  , Pretty
  , align
  , concatWith
  , defaultLayoutOptions
  , indent
  , layoutPretty
  , line
  , pretty
  , (<+>)
  )
import Prettyprinter.Render.String (renderString)


defaultIndentationSpaces :: Int
defaultIndentationSpaces = 4


pString :: String -> Doc ann
pString = pretty


-- | Add 4 to document indentation and put a line before the document.
indentPretty :: Pretty a => a -> Doc ann
indentPretty x = indent defaultIndentationSpaces (line <> pretty x)


prettyWithHeader :: Pretty a => String -> a -> Doc ann
prettyWithHeader header value =
  pretty header
    <> line
    <> indentPretty value


render :: Pretty a => a -> String
render x =
  renderDoc (pretty x)


renderDoc :: Doc ann -> String
renderDoc doc =
  renderString $ layoutPretty defaultLayoutOptions doc


prettyItemList
  :: forall x y ann
   . Pretty x
  => Pretty y
  => [(x, y)]
  -> Doc ann
  -> Doc ann
  -> Doc ann
prettyItemList items sep binder =
  let
    prettyItem :: (x, y) -> Doc ann
    prettyItem (li, ri) =
      pretty li
        <+> binder
        <+> pretty ri
   in
    (align <<< indent defaultIndentationSpaces)
      ( concatWith
          ( \x y ->
              x <> line <> sep <+> y
          )
          (prettyItem <$> items)
      )


throwDocError
  :: Error String :> es
  => Doc ann
  -> Eff es a
throwDocError = throwError <<< renderDoc


throwPrettyError
  :: Error String :> es
  => Pretty a
  => a
  -> Eff es a
throwPrettyError = throwError <<< render
