module Main where

import Data.Char (toUpper)
import Text.Megaparsec (oneOf, some)
import Utils

main :: IO ()
main =
    run $
        Config
            { parser = some (oneOf "abc")
            , parts =
                [ print
                , print . map toUpper
                ]
            }
