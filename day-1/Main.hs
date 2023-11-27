module Main where

import Utils

main :: IO ()
main = run parser parts
  where
    parser = some (integer <* eol)
    parts =
        [ print
        , print . sum
        ]
