module Day5 where

import Data.List

main = do
  input <- readFile "../input/day05"
  let info = getSeat <$> lines input
  print $ part1 info
  print $ part2 info

binToInt l = sum $ map (2^) $ findIndices (==1) $ reverse l

getSeat str = binToInt $ map getBin str
  where getBin l = if l == 'F' || l == 'L' then 0 else 1

part1 = maximum
part2 info = sum [minimum info..maximum info] - sum info
