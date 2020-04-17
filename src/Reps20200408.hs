module Reps20200408 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOflist =
  case listOflist of
    [] -> []
    (x:xs) ->
      x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe  mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a -> [b]) -> [a] -> [b]
bindListFlip func listA =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindListFlip func xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindList xs func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOflist =
  bindList listOflist id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func =
  joinMaybe $ fmap func ma

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth stringName =
  case stringName of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

valNumber :: String -> Maybe Int
valNumber stringInt =
  readMaybe stringInt

valPositive :: Int -> Maybe Int
valPositive int =
  if int>(0::Int)
     then Just int
     else Nothing

valAge :: String -> Maybe Age
valAge stringAge =
 Age <$> joinMaybe (valPositive <$> valNumber stringAge)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) string =
  if ageInt>(18::Int)
     then Just string
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bm) stringName =
  case bm of
    "" -> Nothing
    (xBm:_) ->
      case stringName of
        "" -> Nothing
        (x:_) ->
          if (xBm==x)
             then Just stringName
             else Nothing

valName :: BirthMonth -> Age -> String -> Maybe Name
valName bm age stringName =
  valLetter bm stringName `bindMaybe` \validLetter ->
  valLength age validLetter `bindMaybe` \validLength ->
  Just $ Name validLength

valNameTwo :: BirthMonth -> Age -> String -> Maybe Name
valNameTwo bm age stringName = do
  validLetter <- valLetter bm stringName
  validLength <- valLength age validLetter
  Just $ Name validLength

valPerson :: String -> String -> String -> Maybe Person
valPerson stringName stringAge stringBm =
  valBirthMonth stringBm `bindMaybe` \validBm ->
  valAge stringAge `bindMaybe` \validAge ->
  valName validBm validAge stringName`bindMaybe` \validName ->
  Just $ Person validName validAge validBm

valPersonTwo :: String -> String -> String -> Maybe Person
valPersonTwo stringName stringAge stringBm = do
  validBm <- valBirthMonth stringBm
  validAge <- valAge stringAge
  validName <- valName validBm validAge stringName
  Just $ Person validName validAge validBm