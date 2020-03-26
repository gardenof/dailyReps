module Reps20200326 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (x:xs) ->
      x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListflip :: (a -> [b]) -> [a] -> [b]
bindListflip func listA =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindListflip func listA

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindList listA func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin mMa func =
  joinMaybe $ fmap func mMa

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth bmString =
  case bmString of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

valInt :: String -> Maybe Int
valInt string =
  readMaybe string

valPositive :: Int -> Maybe Int
valPositive int =
  if int<(0::Int)
     then Nothing
     else Just int

valAge :: String -> Maybe Age
valAge stringAge =
  Age <$> joinMaybe
            (valPositive <$> valInt stringAge)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) string =
  if ageInt<(18::Int)
     then Just string
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bmString) nameString =
  case bmString of
    (bx:_) ->
      case nameString of
        (x:_) ->
          if (bx==x)
             then Just nameString
             else Nothing

valName :: BirthMonth -> Age -> String -> Maybe Name
valName bm age nameString =
  valLetter bm nameString `bindMaybe` \validLetter ->
    valLength age validLetter `bindMaybe` \validLength ->
      Just $ Name validLength

valPerson :: String -> String -> String -> Maybe Person
valPerson nameString ageString bmString =
  valBirthMonth bmString `bindMaybe` \validBm ->
    valAge ageString `bindMaybe` \validAge ->
      valName validBm validAge nameString `bindMaybe` \validName ->
        Just $ Person validName validAge validBm
