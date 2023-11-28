module Main where

import Aoc

data Input = Input Int Int Int deriving (Show)

main :: IO ()
main = run parser parts
  where
    parser = Input <$> nextInt <*> nextInt <*> nextInt
    nextInt = integer <* eol

    parts = [print, print . sum3]
    sum3 (Input a b c) = a + b + c
