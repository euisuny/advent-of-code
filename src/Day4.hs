module Day4 where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

-- An ugly day, but it works..
parse :: String -> [[String]]
parse = map words . lines

parse' :: [[String]] -> [[(String, String)]]
parse' = map (map $ parseTrunc parser)

parseTrunc :: ReadP a -> String -> a
parseTrunc parser input =
  case readP_to_S parser input of
      ((result, _):_) -> result

parser :: ReadP (String,String)
parser = do
  c <- munch isAlpha
  char ':'
  str <- munch $ not . isSpace
  return (c, str)

isValid :: (String, String) -> Bool
isValid (x, y) =
  case (x, y) of
    ("byr", _) -> all (isDigit) y && length y == 4 && 1920 <= read y && read y <= 2002
    ("iyr", _) -> all (isDigit) y && length y == 4 && 2010 <= read y && read y <= 2020
    ("eyr", _) -> all (isDigit) y && length y == 4 && 2020 <= read y && read y <= 2030
    ("hgt", _) ->
      ((drop (length y - 2) y == "cm" && (((read :: String -> Int) $ take (length y - 2) y) >= 150))
        && (((read :: String -> Int) $ take (length y - 2) y) <= 193))
        || ((drop (length y - 2) y == "in" &&
              (((read :: String -> Int) $ take (length y - 2) y) >= 59))
        && (((read :: String -> Int) $ take (length y - 2) y) <= 76))
    ("hcl", _) -> length y == 7 && y !! 0 == '#' && all isHexDigit (drop 1 y)
    ("ecl", "amb") -> True
    ("ecl", "blu") -> True
    ("ecl", "brn") -> True
    ("ecl", "gry") -> True
    ("ecl", "grn") -> True
    ("ecl", "hzl") -> True
    ("ecl", "oth") -> True
    ("pid", _) -> length y == 9 && all (isDigit) y
    ("cid", _) -> True
    _ -> False

main = do
  input <- readFile "../input/day04"
  print $ length $
    filter (\x -> ((sort $ map fst x) == ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"] ||
                   (sort $ map fst x) == ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"])
                && all isValid x) $ (parse' $ parse input)
