module Year2020.Month04.Reps20200402 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOflist =
  case listOflist of
    [] -> []
    (x:xs) -> x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a -> [b]) -> [a] -> [b]
bindListFlip func listA =
  case listA of
    [] -> []
    (x:xs) ->  func x <> bindListFlip func xs

bindList :: [a] -> (a -> [b]) -> [b]
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
valName bm age stringName = do
  validLength <- valLength age stringName
  validLetter <-  valLetter bm validLength
  Just $ Name validLetter

valNameTwo :: BirthMonth -> Age -> String -> Maybe Name
valNameTwo bm age stringName =
  valLength age stringName >>= \validLength ->
  valLetter bm validLength >>= \validLetter ->
  Just $ Name validLetter

valPerson :: String -> String -> String -> Maybe Person
valPerson stringName stringAge stringBm = do
  validBm <- valBirthMonth stringBm
  validAge <- valAge stringAge
  validName <-  valName validBm validAge stringName
  Just $ Person validName validAge validBm

valPersonTwo :: String -> String -> String -> Maybe Person
valPersonTwo stringName stringAge stringBm =
  valBirthMonth stringBm >>= \validBm ->
  valAge stringAge >>= \validAge ->
  valName validBm validAge stringName >>= \validName ->
  Just $ Person validName validAge validBm