module Notate.Dummy
  ( makeDummyKernel
  ) where

import Control.Monad.State (gets)
import qualified Data.Text as T
import IHaskell.IPython.EasyKernel (KernelConfig(..))
import IHaskell.IPython.Types
import Notate.Core
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

makeDummyKernel :: NotateM Kernel
makeDummyKernel = do
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
