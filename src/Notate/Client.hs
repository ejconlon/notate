{-# LANGUAGE OverloadedStrings #-}

module Notate.Client
  ( DisplayData(..)
  , emitText
  , emitDisplayData
  , hasDisplayData
  , readAllDisplayData
  , clearAllDisplayData
  ) where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Monoid (mempty)
import Data.HashMap.Strict as HMS
import Data.Text (Text)
import System.Environment (lookupEnv)

data DisplayData = DisplayData
  { ddData :: A.Value
  , ddMetadata :: A.Value
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

emitText :: MonadIO m => Text -> Text -> m ()
emitText ty con  = emitDisplayData dd
  where
    dat = A.Object (HMS.singleton ty (A.String con))
    meta = A.Object (HMS.empty)
    dd = DisplayData dat meta

withMagicFile :: MonadIO m => (FilePath -> m ()) -> m ()
withMagicFile act = do
  mf <- liftIO $ lookupEnv magicFileKey
  case mf of
    Nothing -> return ()
    Just file -> act file

emitDisplayData :: MonadIO m => DisplayData -> m ()
emitDisplayData dd = undefined

hasDisplayData :: MonadIO m => m Bool
hasDisplayData = undefined

readAllDisplayData :: MonadIO m => m [DisplayData]
readAllDisplayData = undefined

clearAllDisplayData :: MonadIO m => m ()
clearAllDisplayData = undefined
