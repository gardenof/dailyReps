module Year2020.Month07.Day14 () where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    x:_ -> Just x
    _   -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail list =
  case list of
    _:xs -> Just xs
    _    -> Nothing

safeLast :: [a] -> Maybe a
safeLast list =
  case list of
    []   -> Nothing
    [a]  -> Just a
    _:xs -> safeLast xs

names :: [String]
names = ["aa","bb","cc","dddd","eeeeeee","ffffffffff"]

lengthOfNames :: [Int]
lengthOfNames =
  map length names

totalLengthOfNames :: Int
totalLengthOfNames =
  sum lengthOfNames

roster :: String
roster =
  List.intercalate ", " names

isLongName :: String -> Bool
isLongName name =
  length name > 7

-- and :: Foldable t => t Bool -> Bool
areAllNamesLong1 :: Bool
areAllNamesLong1 =
  and (map isLongName names)

-- all :: Foldable t => (a -> Bool) -> t a -> Bool
areAllNamesLong2 :: Bool
areAllNamesLong2 =
  all isLongName names

-- or :: Foldable t => t Bool -> Bool
anyNameIsLong1 :: Bool
anyNameIsLong1 =
  or (map isLongName names)

-- any :: Foldable t => (a -> Bool) -> t a -> Bool
anyNameIsLong2 :: Bool
anyNameIsLong2 =
  any isLongName names

-- filter :: (a -> Bool) -> [a] -> [a]
justLongNames :: [String]
justLongNames =
  filter isLongName names

-- partition :: (a -> Bool) -> [a] -> ([a], [a])
longNames, shortNames :: [String]
(longNames, shortNames) =
  List.partition isLongName names

isDarrylLong :: Bool
isDarrylLong =
  elem "darryl" longNames

aLongName :: Maybe String
aLongName =
  List.find isLongName names

shortenName :: String -> String
shortenName = take 7

remainingName :: String -> String
remainingName = drop 7

shortenNameWithRemainder :: String -> (String, String)
shortenNameWithRemainder = splitAt 7

isEnglishVowel :: Char -> Bool
isEnglishVowel char =
  elem char "AaEeIiOoUu"

dropLeadingVowels :: String -> String
dropLeadingVowels =
  dropWhile isEnglishVowel

takeLeadingVowels :: String -> String
takeLeadingVowels =
  takeWhile isEnglishVowel

splitOffLeadingVowels :: String -> (String, String)
splitOffLeadingVowels =
  span isEnglishVowel

splitOffLeadingConsonants :: String -> (String,String)
splitOffLeadingConsonants =
  break isEnglishVowel
