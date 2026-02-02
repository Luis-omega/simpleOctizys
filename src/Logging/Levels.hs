{-# LANGUAGE DeriveLift #-}

module Logging.Levels
  ( Level (Error, Warn, Info, Debug, Trace)
  ) where

import Language.Haskell.TH.Syntax (Lift)

import Control.Arrow ((<<<))
import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (toEncoding)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty))


data Level = Error | Debug | Info | Trace | Warn
  deriving (Show, Eq, Lift)
  deriving (Generic)


instance ToJSON Level where
  toEncoding = genericToEncoding defaultOptions


instance Ord Level where
  _ <= Error = True
  Error <= _ = False
  _ <= Warn = True
  Warn <= _ = False
  _ <= Info = True
  Info <= _ = False
  _ <= Debug = True
  Debug <= _ = False
  Trace <= _ = True


instance Pretty Level where
  pretty = pretty <<< show
