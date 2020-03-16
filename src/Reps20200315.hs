module Reps20200315 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] ->[]
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
bindMaybe ma functionA2ListB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2ListB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe ( Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA functionA2ListB =
  joinList $ fmap functionA2ListB listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma functionA2ListB =
  joinMaybe $ fmap functionA2ListB ma

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
validateNameTwo birthMonth age string =
  Name <$> (joinMaybe (validateAgeLength age <$> validateBmAndNameLetter birthMonth string))


{-
##### Monad
##### Part A
join       :: Monad m => m (m a) -> m a
Bind (>>=) :: m a -> (a -> m b) -> m b
"I'd specifically like you to write bind with the arguments flipped,
  so that it mirrors Functor and Applicative"

Write join for List
Write join for Maybe
Write Bind for List
Write Bind for Maybe

Write join for list useing Bind
Write join for Maybe useing Bind
Write Bind for list useing join
Write Bind for Maybe useing join

##### Part B
(=<<) :: (a -> mb) -> ma -> mb "fliped bind"
(>>=) :: ma -> (a -> mb) -> mb

1. validate Name     :: String -> Maybe Name {Can't be blank "" }
2. validate Number   :: String -> Maybe Int {use read Maybe}
3. validate Positive :: Int -> Maybe Int
4. validate Age      :: String -> Maybe Age {useing 2 & 3}
5. validate Person   :: String -> String -> Maybe Person {do useing 1 & 4}

##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate NameVtwo :: BirthMonth -> Age -> String -> Maybe Name

validate PersonVtwo   :: String -> String -> Maybe Person {do useing 1 & 4}

Name Validators
Must start with same letter as BirthMonth
Name Must be 2 words if age is over 18

-}
