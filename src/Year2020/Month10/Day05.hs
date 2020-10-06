module Year2020.Month10.Day05 where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x:_ -> Just x

safeTail :: [a] -> Maybe [a]
safeTail list =
  case list of
    [] -> Nothing
    _:xs -> Just xs

safeLast :: [a] -> Maybe a
safeLast list =
  case list of
    []   -> Nothing
    [x]  -> Just x
    _:xs -> safeLast xs

names :: [String]
names = ["asd","asds","qdas","asdq"]

lengthOfNames :: [Int]
lengthOfNames = map length names

totalLengthOfNames :: Int
totalLengthOfNames = sum lengthOfNames

roaster :: String
roaster = List.intercalate ", " names

isLongName :: String -> Bool
isLongName name = length name > 7

areAllNamesLong1 :: Bool
areAllNamesLong1 = and (map isLongName names)

areAllNamesLong2 :: Bool
areAllNamesLong2 = all isLongName names

anyNameIsLong1 :: Bool
anyNameIsLong1 = or (map isLongName names)

anyNameIsLong2 :: Bool
anyNameIsLong2 = any isLongName names

justLongNames :: [String]
justLongNames = filter isLongName names

longNames, shortNames :: [String]
(longNames, shortNames) =
  List.partition isLongName names

isDarrylLong :: Bool
isDarrylLong = elem "darryl" longNames

aLongName :: Maybe String
aLongName = List.find isLongName names

shortName :: String -> String
shortName = take 7

remainingName :: String -> String
remainingName = drop 7

shortenNameWithRemainder :: String -> (String,String)
shortenNameWithRemainder = splitAt 7

isEnglishVowel :: Char -> Bool
isEnglishVowel char = elem char "AaEeIiOoUu"

dropLeadingVowels :: String -> String
dropLeadingVowels = dropWhile isEnglishVowel

takeLeadingVowels :: String -> String
takeLeadingVowels = takeWhile isEnglishVowel

splitOffLeadingVowels :: String -> (String, String)
splitOffLeadingVowels = span isEnglishVowel

splitOffLeadingConsonants :: String -> (String, String)
splitOffLeadingConsonants = break isEnglishVowel
