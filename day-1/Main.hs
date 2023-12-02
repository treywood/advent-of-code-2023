module Main where

import Aoc

main :: IO ()
main = run parser parts
  where
    parser = sepEndBy (til int *> some (int <* til (int <|> newline))) newline
    til p = manyTill letterChar (try $ lookAhead p)
    int =
        choice
            [ digitChar
            , '1' <$ stringRewind "one"
            , '2' <$ stringRewind "two"
            , '3' <$ stringRewind "three"
            , '4' <$ stringRewind "four"
            , '5' <$ stringRewind "five"
            , '6' <$ stringRewind "six"
            , '7' <$ stringRewind "seven"
            , '8' <$ stringRewind "eight"
            , '9' <$ stringRewind "nine"
            ]
    stringRewind str = lookAhead (string str) *> char (head str)

    parts = [print . sum . map firstLast]

    firstLast :: String -> Int
    firstLast digits = read [head digits, last digits]
