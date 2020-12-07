{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Day7 where

import Control.Lens
import Control.Monad
import Data.Char

bag = "shinygold"

main = do
  input <- readFile "../input/day07"
  let info = parse $ words <$> lines input
  print $ part1 info
  print $ part2 info

part1 = sum . ap ((<$>) . (fromEnum <$>) . flip part1' bag) (fst <$>)
part2 = subtract 1 . flip part2' bag

part1' :: [(String, [(String, String)])] -> String -> String -> Bool
part1' l find str = case filter (\(x, _) -> x == str) l ^? (element 0) of
                    Just (_, result) -> comp result
                    _ -> False
  where comp [] = False
        comp ((n, x) : l') = (x == find) || (part1' l find x) || comp l'

part2' :: [(String, [(String, String)])] -> String -> Int
part2' l str = let (_, result) = (filter (\(x, y) -> x == str) l) !! 0 in
            comp result
  where comp [] = 1
        comp ((n, x) : l') = case read n of
          0 -> 1
          _ -> (read n) * (part2' l x) + comp l'

parse' [] = []
parse' (x : y : l) = (x, y) : parse' l
parse' (x : l) = ("0", "noother") : parse' l

parse l = map (\x -> (x !! 0, parse' $ drop 1 x)) l
