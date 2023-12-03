module Main where

import Aoc

data Game = Game
    { num :: Int
    , draws :: [[(Int, Cube)]]
    }
    deriving (Show)

data Cube = Red | Blue | Green deriving (Show, Eq)

main :: IO ()
main = run parser parts
  where
    parser = sepEndBy game newline
    game =
        Game
            <$> (string "Game " *> integer <* char ':' <* space)
            <*> sepBy draw (char ';' <* space)
    draw = sepBy ((,) <$> integer <* space <*> cube) (char ',' <* space)
    cube =
        choice
            [ Red <$ string "red"
            , Blue <$ string "blue"
            , Green <$ string "green"
            ]

    parts =
        [ print . sum . map num . filter possible
        , print . sum . map power
        ]

    -- part 1
    possible :: Game -> Bool
    possible (Game _ draws) = all (all good) draws

    good (n, Red) = n <= 12
    good (n, Green) = n <= 13
    good (n, Blue) = n <= 14

    -- part 2
    power :: Game -> Int
    power (Game _ draws) = maxOf Red * maxOf Green * maxOf Blue
      where
        maxOf :: Cube -> Int
        maxOf c = maximum $ map (sum . map fst . filter ((== c) . snd)) draws
