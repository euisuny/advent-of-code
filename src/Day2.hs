module Day2 where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

main = do
  input <- readFile "../input/day02"
  let parse = map (parseMaybe parser) . lines
  print $ solve part1 $ parse input
  print $ solve part2 $ parse input

-- Parsing
parseMaybe :: ReadP a -> String -> a
parseMaybe parser input =
    case readP_to_S parser input of
        ((result, _):_) -> result

natural :: ReadP Int
natural = read <$> some (satisfy isDigit)

parser :: ReadP (Int, Int, Char, String)
parser = do
  low <- natural
  char '-'
  high <- natural
  skipSpaces
  c <- satisfy isAlpha
  char ':'
  skipSpaces
  str <- munch isAlpha
  return (low, high, c, str)

-- Solution
part1 :: (Int, Int, Char, String) -> Bool
part1 (low, high, c, str) = occ >= low && occ <= high
  where occ = length (filter (\x -> x == c) str)

part2 :: (Int, Int, Char, String) -> Bool
part2 (i, j, c, str) =
  ((fromEnum $ str ^? element (i-1) == Just c) +
   (fromEnum $ str ^? element (j-1) == Just c)) == 1

solve isValid = length . filter isValid
