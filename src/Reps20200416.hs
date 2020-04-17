module Reps20200416 () where

import Text.Read (readMaybe)

joinList :: [[a]] -> [a]
joinList listOfList =
  case listOfList of
    [] -> []
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
    (x:xs) ->
      func x <> bindListFlip func xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) ->
      func x <> bindList listA func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe listA func =
  case listA of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfList =
  bindList listOfList id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma func =
  joinMaybeWithBind $ fmap func ma

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth bmString =
  case bmString of
    "" -> Nothing
    anyStr ->
      Just $ BirthMonth anyStr

valNumber :: String -> Maybe Int
valNumber nameString =
  readMaybe nameString

valPositive :: Int -> Maybe Int
valPositive  int =
  if int>(0::Int)
    then
      Just int
    else
      Nothing

valAge :: String -> Maybe Age
valAge ageString =
  Age <$> joinMaybe ( valPositive <$> valNumber ageString)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) str =
  if ageInt>(18::Int)
     then Just str
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bmString) str =
  case bmString of
    "" -> Nothing
    (xBm:_) ->
      case str of
        "" -> Nothing
        (x:_) ->
          if (xBm==x)
             then Just str
             else Nothing

valName :: BirthMonth -> Age -> String -> Maybe Name
valName bm age str = do
  validLetter <- valLetter bm str
  validLength <- valLength age validLetter
  Just $ Name validLength

valPerson :: String -> String -> String -> Maybe Person
valPerson nameStr ageStr bmStr = do
  validBm <- valBirthMonth bmStr
  validAge <- valAge ageStr
  validName <- valName validBm validAge nameStr
  Just $ Person validName validAge validBm

{-
  let
    name = expression
  in
    expression using name

    expression give a valuew
    Must be vaild when () are around it

   -}

eight :: Int
eight =
  let
    four = 4
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
    five = 5
    two = 2
  in
    five * two

tenV2 :: Int
tenV2 =
  let
    five =5
  in
    let
      two =2
    in
      five * two

eightLamda :: Int
eightLamda =
  (\eight -> eight*1)8

tenLamda :: Int
tenLamda =
  (\five two -> five*two)5 2

calFive :: Int
calFive =
  let
    two = 2
    four = two * two
    one = 1
    five = four + one
  in
    five

calFiveV2 :: Int
calFiveV2 =
  let
    five = four + one
    four = two * two
    two  = 2
    one  = 1
  in
    five

buildOnes :: [Int]
buildOnes =
  let
    ones = 1:ones
  in
    take 1000 ones

buildInfinite :: [Int]
buildInfinite =
  let
    oneAndZero = 1:zeroAndOne
    zeroAndOne = 0:oneAndZero
  in
    oneAndZero

justTen :: Maybe Int
justTen = do
  one <- Just 1
  four <- Just 4
  let two = 2
      three = 3
  Just (one+two+three+four)



{-
##### do
- Construct a Maybe Integer value as usual
- Demonstrate a trivial use a do with Maybe defining the same value as above
- Demonstrate a similar trivial use of do with List
- Demonstrate a trivial polymorphic use of do using `pure` (i.e. one that works for all Applicatives)
- Use do syntax to define a function that adds 1 to a `Maybe Integer`
- Use do syntax to define a function that adds takes a list of Integers and adds 1 and 2 to each one, creating a single list of results
- Use do syntax to define a polymorphic function that adds 1 to an Integer within any Monad
- Use do syntax to define a function that takes two lists and returns a list that contains the sums and products of all the combinations of items from the two list.
- Use do syntax to define a function that adds two Integer values within any Monad
- Use do syntax to define a function that returns the pair of the product and sum of two Integer within a Monad
  * use let inside the two to assign variables names for the sum and product
- Define an alias for `getLine` with the correct type signature
+ Define an IO operation that finds the length of a line of input from the user
  - once using do syntax
  - once using fmap
- Use fmap to do the same thing
- Define an alias for `putStrLn` with the correct type signature
+ Define a function that takes a string and builds an IO that show it as a prompt on the screen, then gets a line of input in three ways:
  - once explicitly i:wq
gnoring the result of printing the prompt (i.e. using `<-`)
  - once implicitly ignoring the result of printing (i.e. without using `<-`)
  - once explicitly pattern matching on the result of printing the line
- Define a function that takes a secret string and a prompt string. It should first
  ask the user to ender the secret. If the secret they enter matches the secret given to
  the function, then use do syntax to show the prompt, get their answer and return it.
  If the secret doesn't match, use do syntax to print a message telling them so
  and then return Nothing from the operation.

RUN BUILD FOR ERRORS
-}
