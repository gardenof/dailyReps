module Reps20200224 () where

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
pureMaybe a = Just a

pureList :: a -> [a]
pureList a = [a]

members :: Maybe [String]
members = Just ["qq","q","q"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunc ma =
  case maybeFunc of
    Nothing -> Nothing
    Just func -> fmap func ma

applyList :: [a -> b] -> [a] -> [b]
applyList listOffuncs listA =
  case listOffuncs of
    [] -> []
    (func:restOfFuncs) ->
      fmap func listA <> applyList restOfFuncs listA

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList functionA2B listA =
  case functionA2B of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restOfFuncs xs

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeList (MakeMaybe String)
                  -> MakeList (MakeMaybe Int)
lengthOfEachMeber listOfMaybeStringOne =
  mapMakeList (mapMakeMaybe length) listOfMaybeStringOne

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"asd",Just"a",Just"sad"]

--lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeStringTwo =
  mapMakeList (mapMakeMaybe length) listOfMaybeStringTwo

data EdenString = EdenString String

justString :: Maybe String
justString = Just "DDE"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringIntInt = EdenStringIntInt String Int Int

justInt :: Maybe Int
justInt = Just 234

createInstanceTwo :: Maybe EdenStringIntInt
createInstanceTwo =
  --applyMaybe (applyMaybe (fmap EdenStringIntInt justString) justInt) justInt
  EdenStringIntInt `fmap` justString `applyMaybe` justInt `applyMaybe` justInt

listOfInt :: [Int]
listOfInt = [1,2,3]

listOfString :: [String]
listOfString = ["12","12","12"]

createInstanceThree :: [EdenStringIntInt]
createInstanceThree =
  --applyList (applyList (fmap EdenStringIntInt listOfString) listOfInt) listOfInt
  EdenStringIntInt `fmap` listOfString `applyList` listOfInt `applyList` listOfInt

createInstancefour :: Maybe EdenString
createInstancefour =
  applyMaybe (pureMaybe EdenString) (pureMaybe "asd")

createInstancefive :: [EdenString]
createInstancefive =
  applyList (pureList EdenString) (pureList "asd")

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe maybeFunc ma =
  case maybeFunc of
    NothingMaybe -> NothingMaybe
    JustMaybe func -> fmap func ma

instance Applicative MakeMaybe where
  pure = pureMakeMaybe
  (<*>) = applyMakeMaybe

pureMakeList :: a -> MakeList a
pureMakeList a = MakeList a NothingList

appendList :: MakeList a -> MakeList a -> MakeList a
appendList listOne listTwo =
  case listOne of
    MakeList x xs ->
      MakeList x (appendList xs listTwo)
    NothingList ->
      case listTwo of
        NothingList -> NothingList
        MakeList x2 xs2 ->
          MakeList x2 (appendList xs2 NothingList)

applyMakeList :: MakeList (a -> b) -> MakeList a -> MakeList b
applyMakeList listOfFuncs listA =
  case listOfFuncs of
    NothingList -> NothingList
    MakeList func restOfFuncs  ->
      appendList (mapMakeList func listA ) (applyMakeList restOfFuncs listA)

instance Applicative MakeList where
  pure = pureMakeList
  (<*>) = applyMakeList

functionComposition :: (b -> c) -> (a -> b) -> a -> c
functionComposition b2c a2b a =
  b2c (a2b a)

functionCompositionB :: (b -> c) -> (a -> b) -> (a -> c)
functionCompositionB b2c a2b =
  \a-> b2c (a2b a)

functionCompositionC :: (b -> c) -> ((a -> b) -> (a -> c))
functionCompositionC b2c =
  \a2b-> \a-> b2c (a2b a)

functionCompositionD :: ((b -> c) -> ((a -> b) -> (a -> c)))
functionCompositionD =
  \b2c -> \a2b-> \a-> b2c (a2b a)

functionCompositionE :: ((b -> c) -> (a -> b) -> a -> c)
functionCompositionE =
  \b2c a2b a-> b2c (a2b a)

getLength :: IO Int
getLength = fmap length getLine

data Car = Car
  { carName :: String
  , carAge  :: String
  }

buildCar :: IO Car
buildCar =
  Car <$> getLine <*> getLine

{-
-}
