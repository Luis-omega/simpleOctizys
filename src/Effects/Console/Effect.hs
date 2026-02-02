{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Console.Effect (Console (PutText, ReadLine), putText, readLine) where

import Data.Text (Text)
import Effectful (Effect)
import Effectful.TH (makeEffect)


data Console :: Effect where
  ReadLine :: Console m Text
  PutText :: Text -> Console m ()


$(makeEffect ''Console)
