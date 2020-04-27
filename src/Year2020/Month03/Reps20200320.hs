module Year2020.Month03.Reps20200320 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    [] -> []
    (list:restOflist) ->
      list <> joinList restOflist

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

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

valAge :: String -> Maybe Age
valAge stringAge =
  Age <$> readMaybe stringAge

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) nameString =
  if ageInt>(18::Int)
     then Just nameString
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
valName birthMonth age stringName =
  valLetter birthMonth stringName `bindMaybe` \valedLetter ->
    valLength age valedLetter `bindMaybe` \valNameString ->
      Just (Name valNameString)

valPerson :: String -> String -> String -> Maybe Person
valPerson stringName stringAge stringBM =
  valBirthMonth stringBM `bindMaybe` \birthMonth ->
  valAge stringAge `bindMaybe` \age ->
  valName birthMonth age stringName `bindMaybe` \name ->
  Just $ Person name age birthMonth
