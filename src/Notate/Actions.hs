module Notate.Actions
  ( runInstall
  , runNotebook
  , runKernel
  , runEval
  ) where

import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (findIndex)
import IHaskell.IPython.EasyKernel (easyKernel, KernelConfig(..))
import IHaskell.IPython.Types
import qualified Language.Haskell.Interpreter as HI
import Notate.Core
import System.Directory
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO
import System.Process

runInstall :: Kernel -> NotateM ()
runInstall kernel = do
  projectDir <- gets nsProjectDir
  configDir <- gets nsConfigDir
  target <- gets nsTarget
  liftIO $ createDirectoryIfMissing False configDir
  let targetDir = configDir </> target
  exists <- liftIO $ doesDirectoryExist targetDir
  if exists
    then fail ("Already exists: " ++ targetDir)
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
      let thisKernelName = languageName (kernelLanguageInfo kernel)
          thisKernelDir = kernelsDir </> thisKernelName
      liftIO $ createDirectory thisKernelDir
      kernelSpec <- liftIO $ writeKernelspec kernel thisKernelDir
      let kernelFile = thisKernelDir </> "kernel.json"
      liftIO $ BL.writeFile kernelFile (A.encode (A.toJSON kernelSpec))

runNotebook :: NotateM ()
runNotebook = do
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

runKernel :: FilePath -> Kernel -> NotateM ()
runKernel profile kernel = do
  liftIO $ putStrLn "starting notate kernel"
  liftIO $ easyKernel profile kernel
  liftIO $ putStrLn "finished notate kernel"

runEval :: NotateM ()
runEval = do
  return ()
