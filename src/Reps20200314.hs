module Reps20200314 () where

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
bindMaybe ma functionA2ListB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2ListB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind  mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA funcL =
  joinList $ fmap funcL listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma funcM =
  joinMaybe $ fmap funcM ma

{-

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
