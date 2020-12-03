module Day3 where

import Data.Char
import Data.List

main :: IO ()
main = do
  input <- readFile "../input/day03"
  let parse = map (map digitToInt) . lines
  let geo = parse input
  print $ part1 geo
  print $ part2 geo

solve :: [[Int]] -> (Int, Int) -> Int
solve geo (x, y) =
  sum [cycle (geo !! y) !! x | (x, y) <- zip [0, x..] [0, y..length geo - 1]]

solver l geo = product $ solve geo <$> l

part1 = solver [(3, 1)]
part2 = solver [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
