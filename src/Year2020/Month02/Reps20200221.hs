module Year2020.Month02.Reps20200221
  () where

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
    (func:restOfFuncs) ->
      fmap func listA <> applyList restOfFuncs listA

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
lengthOfEachMeber maybeListOfString =
  mapMakeMaybe (mapMakeList length) maybeListOfString

justListOfString :: [Maybe String]
justListOfString = [Just"sdf",Just"sdf",Just"adf"]

lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeString =
  mapMakeList (mapMakeMaybe length) listOfMaybeString

data EdenString = EdenString String

justString :: Maybe String
justString = Just"asdf"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringIntInt = EdenStringIntInt String Int Int

justInt :: Maybe Int
justInt = Just 5

createInstanceTwo :: Maybe EdenStringIntInt
createInstanceTwo =
  applyMaybe (applyMaybe (fmap EdenStringIntInt justString) justInt) justInt

createInstanceTwoB :: Maybe EdenStringIntInt
createInstanceTwoB =
  ((fmap EdenStringIntInt justString) `applyMaybe` justInt) `applyMaybe` justInt

listofString :: [String]
listofString = ["fgdf","wersf","wer","asfd"]

listOfInt :: [Int]
listOfInt = [1,2,3,4,5]

createInstanceThree :: [EdenStringIntInt]
createInstanceThree =
  applyList (applyList (fmap EdenStringIntInt listofString) listOfInt) listOfInt

createInstanceThreeB :: [EdenStringIntInt]
createInstanceThreeB =
  ((fmap EdenStringIntInt listofString) `applyList` listOfInt) `applyList` listOfInt

-- Now right that with out fmap. Use PureMaybe and applyMaybe maybe.
createInstancefour :: Maybe EdenString
createInstancefour =
  applyMaybe (Just EdenString) (pureMaybe "lkjl")

-- Now that you wrote one for Myabe write one for List
createInstancefive :: [EdenString]
createInstancefive =
  applyList [EdenString] (pureList "werc")

pureMakeMaybe :: a -> MakeMaybe a
pureMakeMaybe a = JustMaybe a

applyMakeMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMakeMaybe maybeFunc ma =
  case maybeFunc of
    NothingMaybe -> NothingMaybe
    JustMaybe func ->
      fmap func ma

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
functionComposition funcB2C funcA2B a=
  funcB2C (funcA2B a)

--Rewrite the fucntions you wrote with infix notation to use operators
createInstanceTwoC :: Maybe EdenStringIntInt
createInstanceTwoC =
  ((EdenStringIntInt <$> justString) <*> justInt) <*> justInt

createInstanceThreeC :: [EdenStringIntInt]
createInstanceThreeC =
  EdenStringIntInt <$> listofString <*> listOfInt <*> listOfInt
