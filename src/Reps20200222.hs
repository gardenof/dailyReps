module Reps20200222 () where

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a -> JustMaybe $ functionA2B a

instance Functor MakeMaybe where
  fmap = mapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs ->
      MakeList (functionA2B x) (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe aa = Just aa

pureList :: a -> [a]
pureList aa = [aa]

members :: Maybe [String]
members = Just ["asdf","asdf","asdf"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunc ma =
  case maybeFunc of
    Nothing -> Nothing
    Just func -> fmap func ma

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (x:xs) ->
      fmap x listA <> applyList xs listA

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restOfFuncs xs

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListOFStringOne =
  mapMakeMaybe (mapMakeList length) maybeListOFStringOne

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"asd",Just"asd",Just"qer"]

--lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo maybeListOFStringTwo =
  mapMakeList (mapMakeMaybe length) maybeListOFStringTwo

data EdenString = EdenString String

justString :: Maybe String
justString = Just"EEf"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringIntInt = EdenStringIntInt String Int Int

justInt :: Maybe Int
justInt = Just 55

createInstanceTwo :: Maybe EdenStringIntInt
createInstanceTwo =
  applyMaybe (applyMaybe (fmap EdenStringIntInt justString) justInt) justInt

createInstanceTwoB :: Maybe EdenStringIntInt
createInstanceTwoB =
  ((EdenStringIntInt `fmap` justString) `applyMaybe` justInt) `applyMaybe` justInt

listofString :: [String]
listofString = ["fsfdg","eee","eee","ee"]

listOfInt :: [Int]
listOfInt = [11,22,33]

createInstanceThree :: [EdenStringIntInt]
createInstanceThree =
  applyList (applyList (fmap EdenStringIntInt listofString) listOfInt) listOfInt

createInstanceThreeB :: [EdenStringIntInt]
createInstanceThreeB =
  ((EdenStringIntInt `fmap` listofString) `applyList` listOfInt) `applyList` listOfInt

createInstancefour :: Maybe EdenString
createInstancefour =
  applyMaybe (pureMaybe EdenString) (pureMaybe "eee")

createInstancefive :: [EdenString]
createInstancefive =
  applyList [EdenString] (pureList "ee")

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe maybeFunc ma =
  case maybeFunc of
    NothingMaybe -> NothingMaybe
    JustMaybe func -> fmap func ma

instance Applicative MakeMaybe where
  pure  = pureMakeMaybe
  (<*>) = applyMakeMaybe


{-

* Rewrite the functions you wrote with infix notation to use operators
* Create code behind the operator (.) Function composition.
  * Create code behind the operator (.) Function composition.
  * write (.) four different times, with 0,1,2,3 Lambdas
  and change "(" in the type to respent the changes

* Build IO Int that gives the length of a string that is typed into getLine. Use the functor instance of IO

* build a data Record with two string fields. Use applicative syntax that will build an IO Record^ by asking for each field to be entered as a line.

* ^^ do both in the REPL.

-}
