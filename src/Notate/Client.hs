{-# LANGUAGE OverloadedStrings #-}

module Notate.Client where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (typeMismatch)
import System.Environment (lookupEnv)

data DisplayData = DisplayData
  { ddData :: A.Object
  , ddMetadata :: A.Object
  } deriving (Show, Eq)

instance A.ToJSON DisplayData where
  toJSON (DisplayData dat meta) = A.object
    [ "data" .= dat
    , "metadata" .= meta
    ]

instance A.FromJSON DisplayData where
  parseJSON (A.Object o) = DisplayData <$> o .: "data" <*> o .: "metadata"
  parseJSON invalid = typeMismatch "DisplayData" invalid

magicFileKey :: String
magicFileKey = "NOTATE_DISPLAY_FILE"

emitDisplayData :: MonadIO m => DisplayData -> m ()
emitDisplayData dd = do
  mf <- liftIO $ lookupEnv magicFileKey
  case mf of
    Nothing -> return ()
    Just file -> undefined
