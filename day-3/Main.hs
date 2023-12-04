module Main where

import Aoc
import Data.List (sort)
import Data.List.Extra (groupOnKey)

data Part = Part Char Int deriving (Show, Eq, Ord)

main :: IO ()
main = run parser parts
  where
    parser = sepEndBy (some printChar) newline

    parts = [print . part1, print . part2]

    part1 = sum . map snd . engineParts
    part2 = sum . map gearRatio . groupOnKey fst . sort . engineParts
      where
        gearRatio (Part '*' _, ns)
            | length ns > 1 = product (map snd ns)
            | otherwise = 0
        gearRatio _ = 0

    engineParts :: [String] -> [(Part, Int)]
    engineParts rows = ps
      where
        (_, _, ps) = foldl search ("", Nothing, []) (zip [0 ..] grid)
        row_len = length (head rows)
        grid = concat rows
        search (n, currPart, nums) (i, c)
            -- new line, inside a number
            | col == 0 && isNum =
                case currPart of
                    Just p -> ([c], adjPart, (p, read n) : nums)
                    _ -> ([c], adjPart, nums)
            -- elsewhere inside a number
            | isNum = (n ++ [c], currPart <|> adjPart, nums)
            -- not inside a number
            | otherwise =
                case currPart of
                    Just p -> ("", Nothing, (p, read n) : nums)
                    _ -> ("", Nothing, nums)
          where
            col = i `mod` row_len
            isNum = c `elem` ['0' .. '9']
            adjPart = case partNeighbors of
                [] -> Nothing
                p : _ -> Just p
            partNeighbors =
                [ Part cj j
                | x <- [-row_len - 1, -row_len, -row_len + 1, -1, 1, row_len - 1, row_len, row_len + 1]
                , let j = i + x
                , j >= 0 && j < length grid
                , let cj = grid !! j
                , cj `notElem` '.' : ['0' .. '9']
                ]
