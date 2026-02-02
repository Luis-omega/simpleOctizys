{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Effects.Console.Interpreter (putLine, runConsole) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret)
import Effects.Console.Effect
  ( Console (PutText, ReadLine)
  , putText
  )
import System.IO (hFlush, stdout)


putLine :: Console :> es => Text -> Eff es ()
putLine s = putText (s <> "\n")


runConsole :: (HasCallStack, IOE :> es) => Eff (Console : es) a -> Eff es a
-- TODO: Add IO error handling
runConsole = interpret $ \_ x -> case x of
  ReadLine -> liftIO Text.getLine
  PutText value -> liftIO $ do
    Text.putStr value
    hFlush stdout
