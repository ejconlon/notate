{-# LANGUAGE OverloadedStrings #-}

module Notate.Client
  ( DisplayData(..)
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (typeMismatch)

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
