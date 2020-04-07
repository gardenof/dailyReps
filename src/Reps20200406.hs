module Reps20200406 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOflist =
  case listOflist of
    [] ->[]
    (x:xs) ->
      x <> joinList xs

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a -> [b]) -> [a] -> [b]
bindListFlip func listA =
  case listA of
    [] -> []
    (x:xs) -> func x <> bindListFlip func xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) -> func x <> bindList xs func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOflist =
  bindList listOflist id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func=
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func=
  joinMaybe $ fmap func ma

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth stringBm =
  case stringBm of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

valNumber :: String -> Maybe Int
valNumber stringInt =
  readMaybe stringInt

valPositive :: Int -> Maybe Int
valPositive int =
  if int>(0::Int)
     then Just int
     else Nothing

valAge :: String -> Maybe Age
valAge stringInt =
  Age <$> joinMaybe ( valPositive <$> valNumber stringInt)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) stringName =
  if ageInt>(18::Int)
     then Just stringName
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bmString) stringName =
  case bmString of
    "" -> Nothing
    (xBm:_) ->
      case stringName of
        "" -> Nothing
        (x:_) ->
          if (xBm==x)
             then Just stringName
             else Nothing

valName :: BirthMonth -> Age -> String -> Maybe Name
valName bm age stringName =
  valLength age stringName >>= \validLength ->
  valLetter bm validLength >>= \validLetter ->
  Just $ Name validLetter

valNameTwo :: BirthMonth -> Age -> String -> Maybe Name
valNameTwo bm age stringName = do
  validLength <- valLength age stringName
  validLetter <- valLetter bm validLength
  Just $ Name validLetter

valPerson :: String -> String -> String -> Maybe Person
valPerson stringName stringAge stringBm =
  valBirthMonth stringBm >>= \validBm ->
  valAge stringAge >>= \validAge ->
  valName validBm validAge stringName >>= \validName ->
    Just $ Person validName validAge validBm

{-
let
  name = expression
in
  expression using name

express must give a value
Must be vauild when () are around it.

-}

eight :: Int
eight =
  let
    four =4
  in
    four * 2

nine :: Int
nine =
  let
    three = 3
  in
    three * three

ten :: Int
ten =
  let
    fiveE = 5
  in
    let
      two = 2
    in
      fiveE * two

tenV2 :: Int
tenV2 =
  let
    fiveA = 5
    two = 2
  in
    fiveA * two

eightLamda :: Int
eightLamda =
  (\eightA -> eightA)8

tenLamda :: Int
tenLamda =
  (\fiveA -> fiveA *2) 5

five :: Int
five =
  let
    two = 2
    four = two * two
    one = 1
    fiveA = four + one
  in
    fiveA

fiveV2 :: Int
fiveV2 =
  let
    fiveA = four + one
    four = two * two
    two = 2
    one = 1
  in
    fiveA

thousandOnes :: [Int]
thousandOnes =
  let
    one = 1:one
  in
    take 1000 one

infinList :: [Int]
infinList =
  let
    oneAndZero = 1:zeroAndOne
    zeroAndOne= 0:oneAndZero
  in
    oneAndZero

justTen :: Maybe Int
justTen = do
  one <- Just 1
  four <- Just 4
  let two = 2
      three = 3
  Just (one+two+three+four)
