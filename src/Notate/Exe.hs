module Notate.Exe where

import Notate.Actions
import Notate.Core
import Notate.Dummy (makeDummyKernel)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)

exe :: IO ()
exe = do
  args <- getArgs
  case args of
    ["kernel", projectDir0, configDir0, target, profile] -> do
      projectDir <- makeAbsolute projectDir0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv projectDir configDir target
      runNotateM (makeDummyKernel >>= runKernel profile) env
    ["notebook", projectDir0, configDir0, target] -> do
      projectDir <- makeAbsolute projectDir0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv projectDir configDir target
      runNotateM runNotebook env
    ["install", projectDir0, configDir0, target] -> do
      projectDir <- makeAbsolute projectDir0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv projectDir configDir target
      runNotateM (makeDummyKernel >>= runInstall) env
    ["eval", projectDir0, configDir0, target] -> do
      projectDir <- makeAbsolute projectDir0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv projectDir configDir target
      undefined
      --runNotateM runEval env
    _ -> do
      putStrLn "Usage:"
      putStrLn "notate install PROJECT_DIR CONFIG_DIR TARGET"
      putStrLn "notate notebook PROJECT_DIR CONFIG_DIR TARGET"
      putStrLn "notate eval PROJECT_DIR CONFIG_DIR TARGET"
      putStrLn "notate kernel PROJECT_DIR CONFIG_DIR TARGET PROFILE_FILE"
