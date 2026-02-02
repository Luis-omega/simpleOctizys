module Logging.Loggers where

import Data.Text (Text)
import Effectful ((:>))
import Effectful.Internal.Monad (Eff)
import Logging.Effect (Log, logEntry)
import Logging.Entry (Field, makeEntry)
import Logging.Levels (Level (Debug, Error, Info, Trace, Warn))

import Prelude hiding (error)


logMessage
  :: Log :> e
  => Level
  -> Text
  -> [Field]
  -> Eff e ()
logMessage lvl msg fields =
  let entry = makeEntry lvl msg fields
   in logEntry entry


error
  :: Log :> e
  => Text
  -> [Field]
  -> Eff e ()
error = logMessage Error


warn
  :: Log :> e
  => Text
  -> [Field]
  -> Eff e ()
warn = logMessage Warn


info
  :: Log :> e
  => Text
  -> [Field]
  -> Eff e ()
info = logMessage Info


debug
  :: Log :> e
  => Text
  -> [Field]
  -> Eff e ()
debug = logMessage Debug


trace
  :: Log :> e
  => Text
  -> [Field]
  -> Eff e ()
trace = logMessage Trace
