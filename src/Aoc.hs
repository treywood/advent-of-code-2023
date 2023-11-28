module Aoc (
    module Aoc.Parser,
    run,
) where

import Aoc.Parser
import Control.Monad (zipWithM_)
import System.Environment (getArgs)

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
        putStrLn $ "\nPart " ++ show ix ++ ":\n"
        fn input
        putStrLn ""
