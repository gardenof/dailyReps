module Year2020.Month07.Day14 () where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    x:_ -> Just x
    _ -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail list =
  case list of
    _:xs -> Just xs
    _ -> Nothing

safeLast :: [a] -> Maybe a
safeLast list =
  case list of
    [a]  -> Just a
    _:xs -> safeLast xs
    []   -> Nothing

names :: [String]
names = ["jim","timmy", "johny", "Smith"]

lengthOfNames :: [Int]
lengthOfNames =
  map length names

totalLengthOfNames :: Int
totalLengthOfNames =
  sum lengthOfNames

roster :: String
roster =
  List.intercalate ", " names

isLongNmae :: String -> Bool
isLongNmae name =
  length name > 7
