{-# LANGUAGE DeriveLift #-}

module Logging.Entry
  ( Field
  , makeField
  , field
  , fieldWith
  , getName
  , getAccurateRepresentation
  , getHumanRepresentation
  , Entry
  , makeEntry
  , getLevel
  , getMessage
  , getFields
  ) where

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON (toJSON), Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types ((.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))
import GHC.List (foldl')
import Language.Haskell.TH.Syntax (Lift)
import Logging.Levels (Level)
import Prettyprinter
  ( Doc
  , Pretty (pretty)
  , defaultLayoutOptions
  , layoutPretty
  )
import Prettyprinter.Render.Text (renderStrict)


data Field = Field'
  { name :: Text
  , accurateRepresentation :: Value
  , humanReadableRepresentation :: Maybe Value
  }
  deriving (Eq, Ord, Show, Lift, Generic)
  deriving (ToJSON) via Generically Field


makeField :: Text -> Value -> Maybe Value -> Field
makeField = Field'


splitField :: Field -> (Aeson.Object, Aeson.Object)
splitField f =
  ( Aeson.KeyMap.singleton
      (Aeson.Key.fromText f.name)
      f.accurateRepresentation
  , Aeson.KeyMap.singleton
      (Aeson.Key.fromText f.name)
      (fromMaybe Aeson.Null f.humanReadableRepresentation)
  )


splitFields :: [Field] -> (Aeson.Value, Aeson.Value)
splitFields fs =
  let
    (accA, accH) =
      foldl'
        ( \(aAcc, hAcc) f ->
            let (aF, hF) = splitField f
             in (Aeson.KeyMap.union aAcc aF, Aeson.KeyMap.union hAcc hF)
        )
        (mempty, mempty)
        fs
   in
    (Aeson.Object accA, Aeson.Object accH)


render :: Doc ann -> Text
render =
  renderStrict
    <<< layoutPretty defaultLayoutOptions


-- | Create field with to json function
fieldWith :: (a -> Value) -> (a -> Doc ann) -> Text -> a -> Field
fieldWith toJson toDoc name value =
  makeField name (toJson value) (Just $ toJSON $ render (toDoc value))


-- | Create fields easily if you have a show instance
field
  :: forall a
   . (Pretty a, ToJSON a)
  => Text
  -> a
  -- ^ field name
  -> Field
-- \| value of the field -> Field
field = fieldWith (toJSON @a) (pretty @a)


getName :: Field -> Text
getName = name


getAccurateRepresentation :: Field -> Value
getAccurateRepresentation = accurateRepresentation


getHumanRepresentation :: Field -> Maybe Value
getHumanRepresentation = humanReadableRepresentation


data Entry = Entry'
  { level :: Level
  , message :: Text
  , fields :: [Field]
  }
  deriving (Eq, Ord, Show, Lift, Generic)


makeEntry :: Level -> Text -> [Field] -> Entry
makeEntry = Entry'


getLevel :: Entry -> Level
getLevel = level


getMessage :: Entry -> Text
getMessage = message


getFields :: Entry -> [Field]
getFields = fields


instance ToJSON Entry where
  toJSON Entry' {..} =
    Aeson.object
      [ "level" .= level
      , "message" .= message
      , "fields" .= humanFields
      , "debug" .= preciseFields
      ]
    where
      (preciseFields, humanFields) = splitFields fields
