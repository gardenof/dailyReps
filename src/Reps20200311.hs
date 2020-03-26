module Reps20200311 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (list:restOfLists) ->
      list <> (joinList restOfLists)

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
    Just a ->
      functionA2MaybeB a

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
bindMaybeWithJoin  ma functionA2MaybeB =
  joinMaybe $ fmap functionA2MaybeB ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    anyName -> Just $Name anyName

validateNumber :: String -> Maybe Int
validateNumber string =
  readMaybe string

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
  { personName :: Name
  , personAge :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson sName sAge =
  pure Person <*> (validateName sName) <*> (validateAge sAge)

newtype BirthMonth = BirthMonth String

validateBmAndNameLetter :: BirthMonth -> String -> Maybe String
validateBmAndNameLetter (BirthMonth bmStrgin) string =
  case bmStrgin of
    [] -> Nothing
    (bx:_) ->
      case string of
        [] -> Nothing
        (x:_) ->
          if (bx == x)
            then Just string
            else Nothing

validateAgeLength :: Age -> String -> Maybe String
validateAgeLength (Age ageInt) string =
  if ageInt>(18::Int)
     then Just string
     else Nothing -- NEEDS MORE
      -- case " " then give string else Nothing

validateNameTwo :: BirthMonth -> Age -> String -> Maybe Name
validateNameTwo birthMonth age string =
  --Name <$> (joinMaybe (validateAgeLength age <$> validateBmAndNameLetter birthMonth string))
  Name <$> (joinMaybe (validateAgeLength age <$> validateBmAndNameLetter birthMonth string))

{-

##### Monad
##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate NameTwo :: BirthMonth -> Age -> String -> Maybe Name

Name Validators
Must start with same letter as BirthMonth
Must be 2 words if age is over 18

V a person 
-}
