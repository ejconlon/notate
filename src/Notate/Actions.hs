module Notate.Actions
  ( install
  , notebook
  , kernel
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import IHaskell.IPython.EasyKernel (easyKernel, KernelConfig(..))
import IHaskell.IPython.Types
import Notate.Core
-- TODO remove this
import Notate.Dummy (makeDummyConfig)
import System.Directory
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Process

install :: NotateM ()
install = do
  projectDir <- gets nsProjectDir
  configDir <- gets nsConfigDir
  target <- gets nsTarget
  config <- makeDummyConfig
  liftIO $ createDirectoryIfMissing False configDir
  let targetDir = configDir </> target
  exists <- liftIO $ doesDirectoryExist targetDir
  if exists
    then error ("Already exists: " ++ targetDir)
    else do
      liftIO $ createDirectory targetDir
      let subConfigDir = targetDir </> "config"
          dataDir = targetDir </> "data"
          runtimeDir = targetDir </> "runtime"
      liftIO $ createDirectory subConfigDir
      liftIO $ createDirectory dataDir
      liftIO $ createDirectory runtimeDir
      let kernelsDir = dataDir </> "kernels"
      liftIO $ createDirectory kernelsDir
      let thisKernelName = languageName (kernelLanguageInfo config)
          thisKernelDir = kernelsDir </> thisKernelName
      liftIO $ createDirectory thisKernelDir
      kernelSpec <- liftIO $ writeKernelspec config thisKernelDir
      let kernelFile = thisKernelDir </> "kernel.json"
      liftIO $ BL.writeFile kernelFile (A.encode (A.toJSON kernelSpec))

notebook :: NotateM ()
notebook = do
  projectDir <- gets nsProjectDir
  configDir <- gets nsConfigDir
  target <- gets nsTarget
  home <- liftIO $ getEnv "HOME"
  let targetDir = configDir </> target
      subConfigDir = targetDir </> "config"
      dataDir = targetDir </> "data"
      runtimeDir = targetDir </> "runtime"
      procDef = CreateProcess
        { cmdspec = ShellCommand ("jupyter notebook")
        , cwd = Just projectDir
        , env = Just
            [ ("HOME", home)
            , ("JUPYTER_CONFIG_DIR", subConfigDir)
            , ("JUPYTER_PATH", dataDir)
            , ("JUPYTER_RUNTIME_DIR", runtimeDir)
            ]
        , std_in = Inherit
        , std_out = Inherit
        , std_err = Inherit
        , close_fds = False
        , create_group = False
        , delegate_ctlc = True
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        }
  (_, _, _, handle) <- liftIO $ createProcess procDef
  exitCode <- liftIO $ waitForProcess handle
  liftIO $ putStrLn ("jupyter exited with " ++ (show exitCode))
  return ()

kernel :: FilePath -> NotateM ()
kernel profile = do
  config <- makeDummyConfig
  liftIO $ putStrLn "starting notate kernel"
  liftIO $ easyKernel profile config
  liftIO $ putStrLn "finished notate kernel"
