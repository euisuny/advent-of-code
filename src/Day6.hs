module Day6 where

import Data.List

main = do
  input <- readFile "../input/day06"
  let info = words <$> lines input
  print $ part1 info
  print $ part2 info

part1 l = sum $ (length . nub) <$> (concat <$> l)
part2 l = sum $ length <$> foldl1 intersect <$> l
