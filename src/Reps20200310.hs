module Reps20200310 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOFlist =
  case listOFlist of
    [] -> []
    (list:restOfLists) ->
      list <> (joinList restOfLists)

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFliped :: (a -> [b]) -> [a] -> [b]
bindListFliped functionA2BList listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2BList x <> bindListFliped functionA2BList xs

bindMaybeFliped :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybeFliped functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a ->
      functionA2B a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindListFliped id listOfLists

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybeFliped id mMa

bindListWithJoin :: (a -> [b]) -> [a] -> [b]
bindListWithJoin functionA2BList listA =
  joinList $ fmap functionA2BList listA

bindMaybeWithJoin :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybeWithJoin functionA2mB ma =
  joinMaybe $ fmap functionA2mB ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    anyName -> Just $ Name anyName

validateNumber :: String -> Maybe Int
validateNumber string =
  readMaybe string

validatePositive :: Int -> Maybe Int
validatePositive int =
  if int<(0::Int)
    then Nothing
    else Just int

newtype Age = Age Int deriving (Eq, Ord, Read, Show)

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> (joinMaybe $ validatePositive <$> validateNumber string)

data Person = Person
  { personName :: Name
  , personAge :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson name age =
  pure Person <*> (validateName name) <*> (validateAge age)

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

validateAgeLength :: Age -> String -> Maybe String
validateAgeLength (Age int) string =
  if int > (18::Int)
    then Just string
    else
      Nothing -- NEEDS MORE

{- commted out to stop error
validateBirthMonthLetter :: BirthMonth -> String -> Maybe String
validateBirthMonthLetter bm string =
  case bm of
    [] -> Nothing
    (bx:_) ->
      case string of
        [] -> Nothing
        (x:_) ->
          if (bx == x)
             then Just string
             else Nothing
-}

--validateNameTwo :: BirthMonth -> Age -> String -> Maybe Name
--validateNameTwo birth age string =

{-
##### Monad
##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate NameTwo :: BirthMonth -> Age -> String -> Maybe Name

Name Validators
Must start with same letter as BirthMonth
Must be 2 words if age is over 18
-}
