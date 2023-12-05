module Main where

import Aoc

data Card = Card
    { wins :: [Int]
    , plays :: [Int]
    , num :: Int
    }
    deriving (Show)

main :: IO ()
main = run parser parts
  where
    parser = sepEndBy card newline
      where
        card =
            Card
                <$ (string "Card" *> space *> integer <* char ':' <* space)
                <*> (sepEndBy1 integer spaces <* char '|' <* spaces)
                <*> sepBy1 integer spaces
                <*> return 1
        spaces = some $ char ' '

    parts = [print . part1, print . sum . map num . part2]

    part1 :: [Card] -> Float
    part1 = sum . map score
      where
        score (Card ws xs _)
            | wn == 0 = 0
            | otherwise = 2 ** (wn - 1)
          where
            wn = fromIntegral $ length $ filter (`elem` ws) xs

    part2 = expand
      where
        expand :: [Card] -> [Card]
        expand [] = []
        expand (c@(Card ws xs n) : cs)
            | wn == 0 = c : expand cs
            | otherwise = c : expand (map add copies ++ rest)
          where
            wn = length $ filter (`elem` ws) xs
            (copies, rest) = splitAt wn cs
            add card@(Card _ _ n') = card {num = n' + n}
