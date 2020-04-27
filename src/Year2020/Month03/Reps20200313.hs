module Year2020.Month03.Reps20200313 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfList =
  case listOfList of
    [] -> []
    (list:restOfLists) ->
      list <> joinList restOfLists

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindlist :: [a] -> (a -> [b]) -> [b]
bindlist listA functionA2ListB =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2ListB x <> bindlist xs functionA2ListB

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma functionA2ListB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2ListB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfList =
  bindlist listOfList id

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindlistwithJoin :: [a] -> (a -> [b]) -> [b]
bindlistwithJoin listA funcl =
  joinList $ fmap funcl listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma funcM =
  joinMaybe $ fmap funcM ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    name -> Just $ Name name

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
  Age <$> (joinMaybe (validatePositive <$> validateNumber string))

data Person = Person
  { namePerson :: Name
  , agePerson :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson nameS ageS =
  pure Person <*> (validateName nameS) <*> (validateAge ageS)

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm



































