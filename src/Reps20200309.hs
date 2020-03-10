module Reps20200309 where

import Text.Read (readMaybe)

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe functionA2MaybeB ma =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2MaybeB a

joinList :: [[a]] -> [a]
joinList listOFlist =
  case listOFlist of
    [] -> []
    (list:restofLists) ->
      list <> joinList restofLists

bindlist :: (a -> [b]) -> [a] -> [b]
bindlist functionA2MaybeB listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2MaybeB x <> bindlist functionA2MaybeB xs

bindListWithJoin :: (a -> [b]) -> [a] -> [b]
bindListWithJoin functionA2MaybeB listA =
  joinList $ fmap functionA2MaybeB listA

bindMaybeWithJoin :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybeWithJoin functionA2MaybeB ma =
  joinMaybe $ fmap functionA2MaybeB ma

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOFlist =
  bindlist id listOFlist

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe id mMa


data Name = Name String deriving Show

validateName :: String -> Maybe Name
validateName name =
  case name of
    "" -> Nothing
    anything -> Just $ Name anything

data Age = Age Int deriving Show

validateNumber :: String -> Maybe Int
validateNumber string =
  readMaybe string

validatePositive :: Int -> Maybe Int
validatePositive num =
  if num < (0::Int)
  then Nothing
  else Just num

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> (joinMaybe $ validatePositive <$> validateNumber string)

data Person = Person
  { personName :: Name
  , personAge :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson name age =
  pure Person <*> validateName name <*> validateAge age

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth birthMonth =
  case birthMonth of
    "" -> Nothing
    anything -> Just $ BirthMonth anything



{-
##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate Name :: BirthMonth -> Age -> String -> Maybe Name

Name Validators
Must start with same letter as BirthMonth
Must be 2 words if age is over 18

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
-}
