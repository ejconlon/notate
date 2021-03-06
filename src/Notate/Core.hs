{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Notate.Core where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.State
import IHaskell.IPython.EasyKernel (KernelConfig(..))
import System.Directory (withCurrentDirectory)

data NotateEnv = NotateEnv
  { nsStackYaml :: FilePath
  , nsConfigDir :: FilePath
  } deriving (Show, Eq)

newtype NotateM a = NotateM
  { unNotateM :: StateT NotateEnv IO a
  } deriving (Functor, Applicative, Monad, MonadState NotateEnv, MonadIO, MonadFail)

type Kernel = KernelConfig IO String String

runNotateM :: NotateM a -> NotateEnv -> IO a
runNotateM = evalStateT . unNotateM
