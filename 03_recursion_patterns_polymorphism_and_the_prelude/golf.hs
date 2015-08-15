module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = map (selectEveryN l) [1..length l]
  where
    selectEveryN :: [a] -> Int -> [a]
    selectEveryN l' n = case drop (n - 1) l' of
                           (y:ys) -> y : selectEveryN ys n
                           []     -> []


localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
  | a < b && b > c = b : localMaxima(c:xs)
  | True           = localMaxima(xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram [] =""
histogram l = unlines $ (transpose bars) ++ ["==========", "0123456789"]
  where
    bars :: [String]
    bars = map(\i -> take (maximum counts - i) (repeat ' ') ++
                     take i (repeat '*')) counts

    counts :: [Int]
    counts = map (\i -> length $ filter (i==) l) [0..9]
