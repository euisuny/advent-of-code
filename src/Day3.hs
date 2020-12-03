module Day3 where

import Control.Lens
import Data.Char
import Data.List

main :: IO ()
main = do
  input <- readFile "../input/day03"
  let parse = map (map digitToInt) . lines
  let geo = parse input
  let a = solve geo (1, 1) (0, 0) 0
  let b = solve geo (3, 1) (0, 0) 0
  let c = solve geo (5, 1) (0, 0) 0
  let d = solve geo (7, 1) (0, 0) 0
  let e = solve geo (1, 2) (0, 0) 0
  -- part1
  print $ b
  -- part2
  print $ (a * b * c * d * e)

nextIndex :: [[Int]] -> (Int, Int) -> (Int, Int) -> Maybe Int
nextIndex geo (dx, dy) (i, j) = do
  row <- geo ^? element (dy + j)
  return $ row !! ((dx + i) `mod` (length $ geo !! 0))

solve geo (dx, dy) (i, j) acc =
  case nextIndex geo (dx, dy) (i, j) of
    Nothing -> acc
    Just val -> solve geo (dx, dy) ((dx + i) `mod` (length $ geo !! 0), dy + j) (acc + val)
