module Utils (
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Control.Applicative,
    run,
    integer,
) where

import Control.Applicative ((*>), (<*), (<*>), (<|>))
import Control.Monad (zipWithM_)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some digitChar

run :: Parser a -> [a -> IO ()] -> IO ()
run parser parts = do
    [inputFile] <- getArgs
    inputStr <- readFile inputFile
    case parse parser inputFile inputStr of
        Left err -> do
            putStrLn "\nParse error:\n"
            putStr (errorBundlePretty err)
            putStrLn ""
        Right input -> do
            zipWithM_ (runPart input) parts [1 ..]
  where
    runPart input fn ix = do
        putStrLn $ "\nPart: " ++ show ix ++ "\n"
        fn input
        putStrLn ""
