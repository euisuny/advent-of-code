module Day6 where

import Control.Monad
import Data.List

main = do
  input <- readFile "../input/day06"
  let info = words <$> lines input
  print $ part1 info
  print $ part2 info

part1 = sum . ((length . nub) <$>) . (join <$>)
part2 = sum . ((length <$> foldl1 intersect) <$>)
