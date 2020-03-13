module Reps20200312 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (list:restOfLists) ->
      list <> joinList restOfLists

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
bindMaybe ma functionA2MaybeB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2MaybeB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA functionA2ListB =
  joinList $ fmap functionA2ListB listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma functionA2MaybeB =
  joinMaybe $ fmap functionA2MaybeB ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName stringName =
  case stringName of
    "" -> Nothing
    anyName -> Just $ Name anyName

validateNumber :: String -> Maybe Int
validateNumber stringNumber =
  readMaybe stringNumber

validatePositive :: Int -> Maybe Int
validatePositive int =
  if int<(0::Int)
    then Nothing
    else Just int

newtype Age = Age Int

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> (joinMaybe ( validatePositive <$> validateNumber string))

data Person = Person
  { namePerson :: Name
  , agePerson :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson  nameS ageS =
  pure Person <*> (validateName nameS) <*> (validateAge ageS)

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    "" -> Nothing
    anyBM -> Just $ BirthMonth anyBM

{-
##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate Name :: BirthMonth -> Age -> String -> Maybe Name

Name Validators
Must start with same letter as BirthMonth
Name Must be 2 words if age is over 18
                        -}
