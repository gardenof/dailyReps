module Year2020.Month08.Day28 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOflists =
  case listOflists of
    [] -> []
    x:xs -> x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a -> [c]) -> [a] -> [c]
bindListFlip func list =
  case list of
    [] -> []
    x:xs -> func x <> bindListFlip func xs

bindList :: [a] -> (a->[b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    x:xs -> func x <> bindList xs func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOflists =
  bindList listOflists id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa = bindMaybe mMa id

bindListWithJoin :: [a] -> (a->[b])->[b]
bindListWithJoin listA func =
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func =
  joinMaybe $ fmap func ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    name -> Just $ Name name

validateNumber :: String -> Maybe Int
validateNumber string = readMaybe string

validatePositive :: Int -> Maybe Int
validatePositive int =
  if int<(0::Int)
     then Nothing
     else Just int

newtype Age = Age Int

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> joinMaybe (validatePositive <$> validateNumber string)

data Person = Person
  { personName :: Name
  , personAge :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson sName sAge =
  pure Person <*> validateName sName <*> validateAge sAge

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    [] -> Nothing
    birthM -> Just $ BirthMonth birthM
