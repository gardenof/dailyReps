module Year2020.Month08.Day29 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOflists =
  case listOflists of
    [] -> []
    x:xs -> x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a-> [b]) -> [a] -> [b]
bindListFlip func listA =
  case listA of
    [] -> []
    x:xs -> func x <> bindListFlip func xs

bindList :: [a] -> (a->[b]) -> [b]
bindList list func =
  case list of
    [] -> []
    x:xs ->
      func x <> bindList xs func

bindMaybe :: Maybe a -> (a-> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOflists =
  bindList listOflists id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a->[b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func =
  joinMaybe $ fmap func ma

newtype Name = Name String

validateName :: String -> Maybe Name
validateName string =
  case string of
    "" -> Nothing
    name -> Just $ Name name

validateNumber :: String -> Maybe Int
validateNumber string = readMaybe string

validatePositive :: Int -> Maybe Int
validatePositive int =
  if int<(0::Int)
     then Nothing
     else Just int

newtype Age = Age Int

validateAge :: String -> Maybe Age
validateAge string =
  Age <$> joinMaybe (validatePositive <$> validateNumber string)

data Person = Person
  { personName :: Name
  , personAge  :: Age
  }

validatePerson :: String -> String -> Maybe Person
validatePerson sName sAge =
  pure Person <*> validateName sName <*> validateAge sAge

newtype BirthMonth = BirthMonth String

validateBirthMonth :: String -> Maybe BirthMonth
validateBirthMonth string =
  case string of
    [] -> Nothing
    birthM -> Just $ BirthMonth birthM

validateBirthMonthAndNameLetter :: BirthMonth -> String -> Maybe String
validateBirthMonthAndNameLetter (BirthMonth bmStrgin) string =
  case bmStrgin of
    [] -> Nothing
    bx:_ ->
      case string of
        [] -> Nothing
        x:_ ->
          if (bx==x)
             then Just string
             else Nothing

validateAgeLength :: Age -> String -> Maybe String
validateAgeLength (Age ageInt) string =
  if ageInt > (18::Int)
     then Just string
     else Nothing

validateNameTwo :: BirthMonth -> Age -> String -> Maybe Name
validateNameTwo birthMonth age rawNameString =
  validateBirthMonthAndNameLetter birthMonth rawNameString
    `bindMaybe` \matchedLetterString ->
      validateAgeLength age matchedLetterString
        `bindMaybe` \goodNameString ->
          pure (Name goodNameString)

validatePersonTwo :: String -> String -> String -> Maybe Person
validatePersonTwo sName sAge sBirthMonth =
  Person <$> joinMaybe
           (   validateNameTwo
           <$> validateBirthMonth sBirthMonth
           <*> validateAge sAge
           <*> Just sName
           )
         <*>
           validateAge sAge
