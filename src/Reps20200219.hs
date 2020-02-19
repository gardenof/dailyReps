module Reps20200219 () where

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

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
    Just func ->
      fmap func ma

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
    (func:restofFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restofFuncs xs

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListString =
  mapMakeMaybe (mapMakeList length) maybeListString

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"adf",Just"asdf",Just"gggg"]

lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeStringTwo =
  mapMakeList (mapMakeMaybe length) listOfMaybeStringTwo

data EdenString = EdenString String

justString :: Maybe String
justString = Just"asd"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringIntInt = EdenStringIntInt String Int Int

justInt :: Maybe Int
justInt = Just 5

createInstanceTwo :: Maybe EdenStringIntInt
createInstanceTwo =
  applyMaybe (applyMaybe (fmap EdenStringIntInt justString) justInt) justInt

listofString :: [String]
listofString = ["asdf","adsf","asdf"]

listOfInt :: [Int]
listOfInt = [1,2,3]

createInstanceThree :: [EdenStringIntInt]
createInstanceThree =
  applyList (applyList (fmap EdenStringIntInt listofString) listOfInt) listOfInt

createInstanceFour :: [EdenStringIntInt]
createInstanceFour =
  (fmap EdenStringIntInt listofString) `applyList` listOfInt  `applyList` listOfInt

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe maybeFunc ma =
  case maybeFunc of
    NothingMaybe -> NothingMaybe
    JustMaybe func ->
      mapMakeMaybe func ma

instance Applicative MakeMaybe where
  pure = pureMakeMaybe
  (<*>)= applyMakeMaybe

{-
createInstance :: Maybe EdenString
Write something with fmap "create insatnce" that only useing fmap.
-- Now right that with out fmap. Use PureMaybe and applyMaybe maybe.
-- Now that you wrote one for Myabe write one for List

Create applicative instance for MakeMaybe and MakeList FOR pure and (<*>)

instance Applicative MakeList where
  pure =
  (<*>) =

-}
