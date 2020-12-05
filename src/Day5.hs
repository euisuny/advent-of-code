module Day5 where

import Data.Bits
import Data.Int
import Data.List

main = do
  input <- readFile "../input/day05"
  let info = getIDs $ lines input
  print $ part1 info
  print $ part2 info

row = (0,128)
column = (0,8)

getIDs info = sort $ map seatID $ map (getSeat row column) info
  where seatID (row, column) = row * 8 + column

getSeat :: (Int, Int) -> (Int, Int) -> String -> (Int, Int)
getSeat (ri, rf) (ci, cf) pt =
  let row_intv = (rf - ri) `div` 2 in
  let col_intv = (cf - ci) `div` 2 in
  case pt of
     [] -> (ri, ci)
     'F' : l -> getSeat (ri, rf - row_intv) (ci, cf) l
     'B' : l -> getSeat (ri + row_intv, rf) (ci, cf) l
     'L' : l -> getSeat (ri, rf) (ci, cf - col_intv) l
     'R' : l -> getSeat (ri, rf) (ci + col_intv, cf) l

part1 = maximum
part2 info = sum [minimum info..maximum info] - sum info
