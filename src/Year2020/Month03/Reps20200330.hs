module Year2020.Month03.Reps20200330 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (x:xs) -> x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindList :: [a] -> (a->[b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) -> func x <> bindList xs func

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

bindListWithJoin :: [a] -> (a->[b]) -> [b]
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
valBirthMonth stringBm =
  case stringBm of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

valNumber :: String -> Maybe Int
valNumber stringNum =
  readMaybe stringNum

valPositive :: Int -> Maybe Int
valPositive int =
  if int>(0::Int)
     then Just int
     else Nothing

valAge :: String -> Maybe Age
valAge stringAge =
  Age <$> joinMaybe (valPositive <$> valNumber stringAge)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) stringName =
  if ageInt>(18::Int)
     then Just stringName
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
  valLength age stringName >>= \validLength ->
  valLetter bm validLength >>= \validLetter ->
  Just $ Name validLetter

valNameTwo :: BirthMonth -> Age -> String -> Maybe Name
valNameTwo bm age stringName = do
  validLength <- valLength age stringName
  validLetter <- valLetter bm validLength
  Just $ Name validLetter

valPerson :: String -> String -> String -> Maybe Person
valPerson nameString ageString bmString =
  valBirthMonth bmString >>= \validBm ->
  valAge ageString >>= \validAge ->
  valName validBm validAge nameString >>= \validName ->
  Just $ Person validName validAge validBm

valPersonTwo :: String -> String -> String -> Maybe Person
valPersonTwo nameString ageString bmString = do
  validBm <- valBirthMonth bmString
  validAge <- valAge ageString
  validName <- valName validBm validAge nameString
  Just $ Person validName validAge validBm
