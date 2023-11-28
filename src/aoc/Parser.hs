module Aoc.Parser (
    module Aoc.Parser,
    module Control.Applicative,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
) where

import Control.Applicative ((*>), (<*), (<*>), (<|>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some digitChar
