module Reps20200225 () where

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
mapMakeList  functionA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs ->
      MakeList
        (functionA2B x)
        (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe a = Just a

pureList :: a -> [a]
pureList a = [a]

members :: Maybe [String]
members = Just ["asd","sdf","sdf"]

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe maybeFunc ma =
  case maybeFunc of
    NothingMaybe -> NothingMaybe
    JustMaybe func -> fmap func ma

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
    MakeList func restOfFuncs ->
      appendList
        (mapMakeList func listA)
        (applyMakeList restOfFuncs listA)

applyZipList :: MakeList (a -> b) -> MakeList a -> MakeList b
applyZipList listOfFuncs listA =
  case listOfFuncs of
    NothingList -> NothingList
    MakeList func restOfFuncs ->
      case listA of
        NothingList -> NothingList
        MakeList x xs ->
          MakeList (func x) (applyZipList restOfFuncs xs)

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListOfString =
  mapMakeMaybe (mapMakeList length) maybeListOfString

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"asd",Just"asd"]

--lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo maybeListOfString =
  mapMakeList (mapMakeMaybe length) maybeListOfString

data Eden = Eden String Int Int

justString :: Maybe String
justString = Just "ASd"

justInt :: Maybe Int
justInt = Just 55

createInstance :: Maybe Eden
createInstance =
  Eden <$> justString <*> justInt <*> justInt

listString :: MakeList String
listString = MakeList "asd" (MakeList "asd" NothingList)

listInt :: MakeList Int
listInt = MakeList 4 (MakeList 5 NothingList)

createInstanceTwo :: MakeList Eden
createInstanceTwo =
  Eden `mapMakeList` listString `applyMakeList` listInt `applyMakeList` listInt

data EdenString = EdenString String

data EdenStringInt = EdenStringInt String Int

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

createInstanceThree :: MakeMaybe EdenStringInt
createInstanceThree =
  applyMakeMaybe (applyMakeMaybe (pureMakeMaybe EdenStringInt) (pureMakeMaybe "asd")) (pureMakeMaybe 5)

instance Applicative MakeMaybe where
  pure = pureMakeMaybe
  (<*>) = applyMakeMaybe

pureMakeList :: a -> MakeList a
pureMakeList a = MakeList a NothingList

instance Applicative MakeList where
  pure = pureMakeList
  (<*>) = applyMakeList

functionComposition :: (b -> c) -> (a -> b) -> a -> c
functionComposition b2c a2b a =
  b2c (a2b a)

functionCompositionA :: (b -> c) -> (a -> b) -> (a -> c)
functionCompositionA b2c a2b =
  \a -> b2c (a2b a)

functionCompositionB :: (b -> c) -> ((a -> b) -> (a -> c))
functionCompositionB b2c =
  \a2b -> \a -> b2c (a2b a)

functionCompositionC :: ((b -> c) -> ((a -> b) -> (a -> c)))
functionCompositionC =
  \b2c -> \a2b -> \a -> b2c (a2b a)

functionCompositionD ::((b -> c) -> (a -> b) -> a -> c)
functionCompositionD =
  \b2c a2b a -> b2c (a2b a)

getLength :: IO Int
getLength =
  fmap length getLine

data Car = Car
  { carName :: String
  , carAge  :: String
  }

buildCar :: IO Car
buildCar =
  (fmap Car getLine) <*> getLine
