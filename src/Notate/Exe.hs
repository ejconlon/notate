module Notate.Exe where

import Notate.Actions
import Notate.Core
import Notate.Real
import System.Directory (makeAbsolute)
import System.Environment (getArgs)

exe :: IO ()
exe = do
  args <- getArgs
  case args of
    ["install", stackYaml0, configDir0] -> do
      stackYaml <- makeAbsolute stackYaml0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv stackYaml configDir
      runNotateM (makeKernel >>= runInstall) env
    ["notebook", stackYaml0, configDir0] -> do
      stackYaml <- makeAbsolute stackYaml0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv stackYaml configDir
      runNotateM runNotebook env
    ["kernel", stackYaml0, configDir0, profile] -> do
      stackYaml <- makeAbsolute stackYaml0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv stackYaml configDir
      runNotateM (makeKernel >>= runKernel profile) env
    ["eval", stackYaml0, configDir0] -> do
      stackYaml <- makeAbsolute stackYaml0
      configDir <- makeAbsolute configDir0
      let env = NotateEnv stackYaml configDir
      runNotateM runEval env
    _ -> do
      putStrLn "Usage:"
      putStrLn "notate install STACK_YAML CONFIG_DIR"
      putStrLn "notate notebook STACK_YAML CONFIG_DIR"
      putStrLn "notate eval STACK_YAML CONFIG_DIR"
      putStrLn "notate kernel STACK_YAML CONFIG_DIR PROFILE_FILE"
