module Notate.Actions
  ( runInstall
  , runNotebook
  , runKernel
  ) where

import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (findIndex)
import IHaskell.IPython.EasyKernel (easyKernel, KernelConfig(..))
import IHaskell.IPython.Types
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

-- stripModules :: String -> String
-- stripModules s =
--   let brackIx = findIndex (== '>') s
--       pipeIx = findIndex (== '|') s
--       lowIx =
--         case (brackIx, pipeIx) of
--           (Nothing, y) -> y
--           (x, Nothing) -> x
--           (x, y) -> min <$> x <*> y
--   in case lowIx of
--     Nothing -> s
--     Just ix -> drop (ix + 1) s

-- type EmitList = [(Int, Int, String)]

-- data LineState = LineState
--   { lsBlock :: !Int
--   , lsLine :: !Int
--   , lsROut :: !EmitList
--   , lsRErr :: !EmitList
--   } deriving (Show, Eq)

-- initLineState :: LineState
-- initLineState = LineState 0 0 [] []

-- runLinesState :: [[String]] -> (String -> IO ()) -> IO [String] -> IO [String] -> StateT LineState IO ()
-- runLinesState inps doIn doOut doErr =
--   case inps of
--     [] -> return ()
--     (i:is) -> do
--       liftIO $ doIn i
--       outs <- liftIO $ doOut
--       errs <- liftIO $ doErr
--       modify $ \s -> s { lsROut = outs : (lsROut s), lsRErr = errs : (lsRErr s) }

-- runLines :: [[String]] -> ([String] -> IO ()) -> IO [String] -> IO [String] -> IO (EmitList, EmitList)
-- runLines inps doIn doOut doErr = do
--   ls <- execStateT (runLinesState inps doIn doOut doErr) initLineState
--   return (reverse (lsROut ls), reverse (lsRErr ls))

-- runIn :: Handle -> [String] -> IO ()
-- runIn handle inp = do
--   hPutStrLn handle ":{"
--   forM_ inp (hPutStrLn handle)
--   hPutStrLn handle ":}"

-- doAppend :: Monad m => [String] -> [String] -> StateT LineState m ()
-- doAppend out err = do
--   block <- gets lsBlock
--   line <- gets lsLine
--   let out' = (\x -> (block, line, stripModules x)) <$> out
--       err' = (\x -> (block, line, x)) <$> err
--   modify $ \s -> s { lsLine = 1 + (lsLine s), lsROut = out' ++ (lsROut s), lsRErr = err' ++ (lsRErr s) }

-- doBlock :: (String -> IO ()) -> IO [String] -> IO [String] -> ([String] -> StateT LineState IO ())
-- doBlock doIn doOut doErr inps = do
--   liftIO $ doIn ":{"
--   out0 <- liftIO $ doOut
--   err0 <- liftIO $ doErr
--   doAppend out0 err0
--   forM_ inps $ \inp -> do
--     liftIO $ doIn inp
--     out <- liftIO $ doOut
--     err <- liftIO $ doErr
--     doAppend out err
--   liftIO $ doIn ":}"
--   out1 <- liftIO $ doOut
--   err1 <- liftIO $ doErr
--   doAppend out1 err1
--   modify $ \s -> s { block = 1 + (block s) }

--runBlocks :: (String -> IO ()) -> IO [String] -> IO [String] -> ([[String]] -> IO (EmitList, EmitList))
--runBlocks doIn doOut doErr inps

-- runEval :: NotateM ()
-- runEval = do
--   projectDir <- gets nsProjectDir
--   home <- liftIO $ getEnv "HOME"
--   path <- liftIO $ getEnv "PATH"
--   let procDef = CreateProcess
--         { cmdspec = ShellCommand ("stack exec intero")
--         , cwd = Just projectDir
--         , env = Just
--             [ ("HOME", home)
--             , ("PATH", path)
--             ]
--         , std_in = CreatePipe
--         , std_out = CreatePipe
--         , std_err = CreatePipe
--         , close_fds = False
--         , create_group = False
--         , delegate_ctlc = False
--         , detach_console = False
--         , create_new_console = False
--         , new_session = False
--         , child_group = Nothing
--         , child_user = Nothing
--         }
--   (Just pstdin, Just pstdout, Just pstderr, handle) <- liftIO $ createProcess procDef
--   liftIO $ putStrLn "Evaluate in intero (blank lines delimit inputs, ^D to run all):"
--   commands <- liftIO $ hGetContents stdin
--   liftIO $ hPutStrLn pstdin ":{"
--   forM_ (lines commands) $ \c ->
--     if (c == "")
--       then do
--         liftIO $ hPutStrLn pstdin ":}"
--         liftIO $ hPutStrLn pstdin ":{"
--       else do
--         liftIO $ hPutStrLn pstdin c
--   liftIO $ hPutStrLn pstdin ":}"
--   liftIO $ hClose stdin
--   out <- liftIO $ hGetContents pstdout
--   err <- liftIO $ hGetContents pstderr
--   exitCode <- liftIO $ waitForProcess handle
--   liftIO $ putStrLn ("intero exited with " ++ (show exitCode))
--   liftIO $ putStrLn "STDOUT"
--   liftIO $ putStrLn ((unlines . (filter (not . null)) . (stripModules <$>) . lines) out)
--   liftIO $ putStrLn "STDERR"
--   liftIO $ putStrLn err
