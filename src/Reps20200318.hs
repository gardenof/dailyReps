module Reps20200318 () where

import Text.Read (readMaybe)

joinlist :: [[a]] -> [a]
joinlist listOfLists =
  case listOfLists of
    [] -> []
    (list:restOfLists) -> list <> joinlist restOfLists

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA functionA2ListB =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2ListB x <> bindList xs functionA2ListB

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma functionA2ListB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2ListB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA functionA2ListB =
  joinlist $ fmap functionA2ListB listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma function =
  joinMaybe $ fmap function ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    name -> Just $ Name name

validateNumber :: String -> Maybe Int
validateNumber string =
  readMaybe string

validatePositive :: Int -> Maybe Int
validatePositive int =
  if int<(0::Int)
     then Nothing
     else Just int

newtype Age =Age Int

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> (joinMaybe ( validatePositive <$> validateNumber string))

data Person = Person
  { personName :: Name
  , personAge :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson sname sage =
  pure Person <*> (validateName sname) <*> (validateAge sage)

newtype BirthMonth =BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    [] -> Nothing
    anyBm -> Just $ BirthMonth anyBm

validateBmAndNameLetter :: BirthMonth -> String -> Maybe String
validateBmAndNameLetter ( BirthMonth bmStrgin) string =
  case bmStrgin of
    [] -> Nothing
    (bx:bxs) ->
      case string of
        [] -> Nothing
        (x:xs) ->
          if (bx==x)
             then Just string
             else Nothing

validateAgeLength :: Age -> String -> Maybe String
validateAgeLength (Age ageInt) string =
  if ageInt>(18::Int)
     then Just string
     else Nothing

validateNameTwo :: BirthMonth -> Age -> String -> Maybe Name
validateNameTwo birthMonth age rawNameString =
  bindMaybe
    (bindMaybe (validateBmAndNameLetter birthMonth rawNameString) (validateAgeLength age))
    (\string -> Just (Name string))


