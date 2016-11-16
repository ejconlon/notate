{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Notate.Core where

import Control.Monad.IO.Class
import Control.Monad.State
import System.Directory (withCurrentDirectory)

data NotateEnv = NotateEnv
  { nsProjectDir :: FilePath
  , nsConfigDir :: FilePath
  , nsTarget :: String
  } deriving (Show, Eq)

newtype NotateM a = NotateM
  { unNotateM :: StateT NotateEnv IO a
  } deriving (Functor, Applicative, Monad, MonadState NotateEnv, MonadIO)

runNotateM :: NotateM a -> NotateEnv -> IO a
runNotateM (NotateM m) env = withCurrentDirectory (nsProjectDir env) (evalStateT m env)