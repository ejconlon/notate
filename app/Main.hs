module Main where

import Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import IHaskell.IPython.EasyKernel (easyKernel, KernelConfig(..))
import IHaskell.IPython.Types
import System.Directory
import System.Environment (getArgs, getEnv)
import System.FilePath
import System.Process
import Text.Parsec
import Text.Parsec.String

-- NOTE(eric) this is just ihaskell test code to be ripped out

-- Define the actual language!
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Exp Expr Expr
          | Val Int
  deriving (Show, Eq)

eval :: Expr -> Int
eval (Val i) = i
eval (Plus x y) = eval x + eval y
eval (Minus x y) = eval x - eval y
eval (Times x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Exp x y) = eval x ^ eval y

parseExpr :: String -> Either String Expr
parseExpr str =
  case parse expr "interactive" (filter (/= ' ') str) of
    Left err -> Left (show err)
    Right e  -> Right e
  where
    expr :: Parser Expr
    expr = val <|> op

    val :: Parser Expr
    val = Val <$> read <$> many1 (oneOf "0123456789")

    op :: Parser Expr
    op = do
      func <- choice $ map string $ ["plus", "minus", "times", "div", "exp"]
      char '('
      x <- expr
      char ','
      y <- expr
      char ')'
      return $
        case func of
          "plus"  -> Plus x y
          "minus" -> Minus x y
          "times" -> Times x y
          "div"   -> Div x y
          "exp"   -> Exp x y

languageConfig :: LanguageInfo
languageConfig = LanguageInfo
  { languageName = "notate"
  , languageVersion = "1.0.0"
  , languageFileExtension = ".hs"
  , languageCodeMirrorMode = "null"
  }

makeKernelSpec :: FilePath -> String -> KernelSpec
makeKernelSpec configDir target = KernelSpec
  { kernelDisplayName = "notate"
  , kernelLanguage = "notate"
  , kernelCommand = ["stack", "exec", "notate", "--", "kernel", configDir, target, "{connection_file}"]
  }

displayString :: String -> [DisplayData]
displayString str = [DisplayData PlainText (T.pack str)]

languageCompletion :: Monad m => T.Text -> Int -> m (T.Text, [T.Text])
languageCompletion code pos = return $
  let (before, _) = T.splitAt pos code
      word = last $ T.words $ T.map replace before
  in (word, map T.pack $ matches $ T.unpack word)
  where
    matches :: String -> [String]
    matches word =
      case head word of
        'p' -> ["plus"]
        'm' -> ["minus"]
        'e' -> ["exp"]
        'd' -> ["div"]
        't' -> ["times"]

    replace :: Char -> Char
    replace '(' = ' '
    replace ')' = ' '
    replace ',' = ' '
    replace x = x

languageInspect :: Monad m => T.Text -> Int -> m (Maybe [DisplayData])
languageInspect _ _ = return $
  Just
    [ DisplayData PlainText $ T.pack $
      unlines
        [ "We support five arithmetic functions:"
        , "   - plus  +"
        , "   - minus -"
        , "   - div   /"
        , "   - times *"
        , "   - exp   ^"
        , "Expressions are written as f(exp, exp)"
        ]
    ]

languageRun :: T.Text -> IO () -> (String -> IO ()) -> IO (String, ExecuteReplyStatus, String)
languageRun code init intermediate = do
  init
  let expr = parseExpr $ T.unpack code
  intermediate (show expr)

  return
    (case expr of
       Left err   -> err
       Right expr -> show (eval expr), IHaskell.IPython.Types.Ok, "")

makeConfig :: FilePath -> String -> KernelConfig IO String String
makeConfig configDir target =
  let kernelSpec = makeKernelSpec configDir target
  in KernelConfig
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

install :: FilePath -> String -> KernelConfig IO output result -> IO ()
install configDir target config = do
  createDirectoryIfMissing False configDir
  let targetDir = configDir </> target
  exists <- doesDirectoryExist targetDir
  if exists
    then error ("Already exists: " ++ targetDir)
    else do
      createDirectory targetDir
      let subConfigDir = targetDir </> "config"
          dataDir = targetDir </> "data"
          runtimeDir = targetDir </> "runtime"
      createDirectory subConfigDir
      createDirectory dataDir
      createDirectory runtimeDir
      let kernelsDir = dataDir </> "kernels"
      createDirectory kernelsDir
      let thisKernelName = languageName (kernelLanguageInfo config)
          thisKernelDir = kernelsDir </> thisKernelName
      createDirectory thisKernelDir
      kernelSpec <- writeKernelspec config thisKernelDir
      let kernelFile = thisKernelDir </> "kernel.json"
      BL.writeFile kernelFile (A.encode (A.toJSON kernelSpec))

notebook :: FilePath -> String -> IO ()
notebook configDir target = do
  home <- getEnv "HOME"
  let targetDir = configDir </> target
      subConfigDir = targetDir </> "config"
      dataDir = targetDir </> "data"
      runtimeDir = targetDir </> "runtime"
      procDef = CreateProcess
        { cmdspec = ShellCommand "jupyter notebook"
        , cwd = Nothing
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
  (_, _, _, handle) <- createProcess procDef
  exitCode <- waitForProcess handle
  putStrLn ("jupyter exited with " ++ (show exitCode))
  return ()

kernel :: FilePath -> String -> FilePath -> KernelConfig IO output result -> IO ()
kernel = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["kernel", configDir, target, profile] -> do
      let config = makeConfig configDir target
      kernel configDir target profile config
    ["notebook", configDir, target] -> do
      notebook configDir target
    ["install", configDir, target] -> do
      let config = makeConfig configDir target
      install configDir target config
    _ -> do
      putStrLn "Usage:"
      putStrLn "notate install CONFIG_DIR TARGET"
      putStrLn "notate notebook CONFIG_DIR TARGET"
      putStrLn "notate kernel CONFIG_DIR TARGET PROFILE_FILE"
