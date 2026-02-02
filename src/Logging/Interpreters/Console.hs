{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Logging.Interpreters.Console
  ( runLog
  ) where

import Control.Arrow ((<<<))
import Control.Monad (when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects.Console.Effect (Console)
import Effects.Console.Interpreter (putLine)
import Logging.Effect (Log (LogEntry))
import Logging.Entry (getFields, getLevel, getMessage)
import Logging.Levels (Level)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import Prettyprinter.Render.Text (renderStrict)


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


inQuotes :: Text -> Doc ann
inQuotes txt =
  pretty '"'
    <> pretty txt
    <> pretty '"'


prettyJSONField :: Text -> Text -> Doc ann
prettyJSONField name value =
  inQuotes name
    <> pretty ':'
    <> inQuotes value


runLog
  :: Console :> es
  => Level
  -> Eff (Log ': es) a
  -> Eff es a
runLog minLevel = interpret $ \_ action ->
  case action of
    LogEntry entry ->
      let
        level = getLevel entry
       in
        when (minLevel <= level) $
          putLine $
            TE.decodeUtf8 $
              BL.toStrict $
                encode entry

-- render
--   ( pretty '{'
--       <> prettyJSONField "level" (Text.pack $ show level)
--       <> pretty ','
--       <> prettyJSONField "message" msg
--       <> pretty ','
--       <> prettyJSONField "fields" (render $ pretty fields)
--       <> pretty '}'
--   )
