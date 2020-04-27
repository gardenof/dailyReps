module Year2020.Month03.Reps20200301 () where

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

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

pureMakeList :: a -> MakeList a
pureMakeList a = MakeList a NothingList

members :: MakeMaybe (MakeList String)
members = JustMaybe (MakeList ("asd") (MakeList ("asd") (NothingList)))

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe  maybeFunction ma =
  case maybeFunction of
    NothingMaybe -> NothingMaybe
    JustMaybe a -> fmap a ma

appendList :: MakeList a -> MakeList a -> MakeList a
appendList listOne listTwo =
  case listOne of
    MakeList x xs ->
      MakeList x ( appendList xs listTwo)
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

lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListOfString =
  mapMakeMaybe (mapMakeList length) maybeListOfString

lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeString =
  mapMakeList (mapMakeMaybe length) listOfMaybeString

data Eden = Eden String Int Int

justString :: MakeMaybe String
justString = JustMaybe "sdf"

justInt :: MakeMaybe Int
justInt = JustMaybe 5

instance Applicative MakeMaybe where
  pure = pureMakeMaybe
  (<*>) = applyMakeMaybe

instance Applicative MakeList where
  pure = pureMakeList
  (<*>) = applyMakeList

createInstance :: MakeMaybe Eden
createInstance =
  pureMakeMaybe Eden <*> pureMakeMaybe "sdf" <*> pureMakeMaybe 4 <*> pureMakeMaybe 4

functionComposition :: (b -> c) -> (a -> b) -> a -> c
functionComposition b2c a2b a =
  b2c (a2b a)

functionCompositionD :: ((b ->c) -> (a -> b) -> a -> c)
functionCompositionD =
  \b2c a2b a -> b2c (a2b a)

getline :: IO Int
getline =
  fmap length getLine

data Car = Car
  {carName :: String
  ,carAge  :: String
  }

build :: IO Car
build =
  (fmap Car getLine) <*> getLine

{-

Create a new type around List
Implement Applicative for it
Provide a pure and apply instance
Make pure be correct for applyZip

Understand "->" in types
figure out the Type for Fmap, Pure, (<*>)
figure out the method for Fmap, Pure, (<*>)
-}
