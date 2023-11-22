{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils (run, Config (..)) where

import Control.Monad (zipWithM_)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void String

data Config a = Config {parser :: Parser a, parts :: [a -> IO ()]}

run :: Config a -> IO ()
run config = do
    [inputFile] <- getArgs
    inputStr <- readFile inputFile
    case parse config.parser inputFile inputStr of
        Left err -> do
            putStrLn "\nParse error:\n"
            putStr (errorBundlePretty err)
            putStrLn ""
        Right input -> do
            zipWithM_ (runPart input) config.parts [1 ..]
  where
    runPart input fn ix = do
        putStrLn $ "\nPart: " ++ show ix ++ "\n"
        fn input
        putStrLn ""
