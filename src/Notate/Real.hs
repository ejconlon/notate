module Notate.Real
  ( makeKernel
  ) where

import Control.Monad.State (gets)
import qualified Data.Text as T
import IHaskell.IPython.EasyKernel (KernelConfig(..))
import IHaskell.IPython.Types
import Notate.Core
import Text.Parsec
import Text.Parsec.String

languageConfig :: LanguageInfo
languageConfig = LanguageInfo
  { languageName = "notate"
  , languageVersion = "1.0.0"
  , languageFileExtension = ".hs"
  , languageCodeMirrorMode = "null"
  }

makeKernelSpec :: NotateM KernelSpec
makeKernelSpec = do
  projectDir <- gets nsProjectDir
  configDir <- gets nsConfigDir
  target <- gets nsTarget
  return KernelSpec
    { kernelDisplayName = "notate"
    , kernelLanguage = "notate"
    , kernelCommand = ["stack", "exec", "notate", "--", "kernel", projectDir, configDir, target, "{connection_file}"]
    }

displayString :: String -> [DisplayData]
displayString str = [DisplayData PlainText (T.pack str)]

languageCompletion :: Monad m => T.Text -> Int -> m (T.Text, [T.Text])
languageCompletion code pos = return (T.empty, [])

languageInspect :: Monad m => T.Text -> Int -> m (Maybe [DisplayData])
languageInspect _ _ = return Nothing

languageRun :: T.Text -> IO () -> (String -> IO ()) -> IO (String, ExecuteReplyStatus, String)
languageRun code init intermediate = do
  init
  let expr = T.unpack code
  intermediate (show expr)
  return (expr, IHaskell.IPython.Types.Ok, "")

makeKernel :: NotateM Kernel
makeKernel = do
  kernelSpec <- makeKernelSpec
  return KernelConfig
    { kernelLanguageInfo = languageConfig
    , writeKernelspec = const (return kernelSpec)
    , displayOutput = displayString
    , displayResult = displayString
    , completion = languageCompletion
    , inspectInfo = languageInspect
    , run = languageRun
    , debug = False
    , kernelBanner = "Notate Haskell Kernel"
    , kernelProtocolVersion = "5.0"
    , kernelImplName = "notate"
    , kernelImplVersion = "0.0"
    }
