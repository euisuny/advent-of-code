module Day1 where

import Data.List

main = do
  input <- readFile "../input/day01"
  let nums = map read . lines
  print $ part1 (nums input)
  print $ part2 (nums input)

year = 2020

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triples :: [a] -> [(a, a, a)]
triples l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thrd3 (_, _, x) = x

part1 :: [Int] -> Int
part1 l = fst pair * snd pair
  where pair = (filter (\x -> fst x + snd x == year) (pairs l)) !! 0

part2 :: [Int] -> Int
part2 l = fst3 trip * snd3 trip * thrd3 trip
  where trip = (filter (\x -> fst3 x + snd3 x + thrd3 x == year) (triples l)) !! 0

