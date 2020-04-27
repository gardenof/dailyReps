module Year2020.Month03.Reps20200323 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (list:restOflist) ->
      list <> joinList restOflist

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindList xs func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func =
  joinMaybe $ fmap func ma

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth birthMonthString =
  case birthMonthString of
    "" -> Nothing
    anyBirthMonth -> Just $ BirthMonth anyBirthMonth

valNumber :: String -> Maybe Int
valNumber stringNum =
  readMaybe stringNum

valPositive :: Int -> Maybe Int
valPositive int =
  if int<(0::Int)
     then Nothing
     else Just int

valAge :: String -> Maybe Age
valAge stringAge =
  Age <$> (joinMaybe
            (valPositive <$> valNumber stringAge)
          )

valLength :: Age -> String -> Maybe String
valLength (Age stringAge) stringName =
  if stringAge>(18::Int)
     then Just stringName
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bmStrgin) nameString =
  case bmStrgin of
    [] -> Nothing
    (bx:_) ->
      case nameString of
        [] -> Nothing
        (x:_) ->
          if (bx==x)
             then Just nameString
             else Nothing

valName :: BirthMonth -> Age -> String -> Maybe Name
valName birthMonth age nameString =
  valLetter birthMonth nameString `bindMaybe` \valedLetter ->
    valLength age valedLetter `bindMaybe` \valNameString ->
      Just (Name valNameString)

valPerson :: String -> String -> String -> Maybe Person
valPerson stringName stringAge stringBM =
  valBirthMonth stringBM `bindMaybe` \birthMonth ->
    valAge stringAge `bindMaybe` \age ->
      valName birthMonth age stringName `bindMaybe` \name ->
        Just $ Person name age birthMonth
