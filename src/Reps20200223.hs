module Reps20200223 (getLength, buildCar) where

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
members = Just ["dd","dd","ddd"]

applyList :: [a -> b] -> [a] -> [b]
applyList listOffuncs listA =
  case listOffuncs of
    [] -> []
    (func:restOfFuncs) ->
      fmap func listA <> applyList restOfFuncs listA

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunc ma =
  case maybeFunc of
    Nothing -> Nothing
    Just func -> fmap func ma

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList listOffuncs listA =
  case listOffuncs of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restOfFuncs xs

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListOFString =
  mapMakeMaybe (mapMakeList length) maybeListOFString

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"ddd",Just"ddd",Just"ddd"]

data EdenString = EdenString String

justString :: Maybe String
justString = Just"JIMJIM"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringIntInt = EdenStringIntInt String Int Int

justInt :: Maybe Int
justInt = Just 123

createInstanceTwo :: Maybe EdenStringIntInt
createInstanceTwo =
  applyMaybe (applyMaybe (fmap EdenStringIntInt justString) justInt) justInt

createInstanceTwoB :: Maybe EdenStringIntInt
createInstanceTwoB =
  ((EdenStringIntInt `fmap` justString) `applyMaybe` justInt) `applyMaybe` justInt

listofString :: [String]
listofString = ["aj","ee","dp","md"]

listOfInt :: [Int]
listOfInt = [9,8,7]

useApplyList :: [EdenStringIntInt]
useApplyList =
  ((EdenStringIntInt `fmap` listofString) `applyList` listOfInt) `applyList` listOfInt

useApplyMaybe :: Maybe EdenString
useApplyMaybe =
  applyMaybe (pureMaybe EdenString) (pureMaybe "eden")

createInstancefive :: [EdenString]
createInstancefive =
  applyList [EdenString] (pureList "dean")

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

useApplyListOps :: [EdenStringIntInt]
useApplyListOps =
  ((EdenStringIntInt <$> listofString) <*> listOfInt) <*> listOfInt

functionComposition :: (b -> c) -> (a -> b) -> a -> c
functionComposition funcB2C funcA2B a=
  funcB2C (funcA2B a)

functionComposition1 :: (b -> c) -> (a -> b) -> (a -> c)
functionComposition1 funcB2C funcA2B =
  \a -> funcB2C (funcA2B a)

functionComposition2 :: (b -> c) -> ((a -> b) -> a -> c)
functionComposition2 funcB2C=
  \funcA2B -> \a-> funcB2C (funcA2B a)

functionComposition3 :: ((b -> c) -> (a -> b) -> a -> c)
functionComposition3 =
  \funcB2C -> \funcA2B -> \a-> funcB2C (funcA2B a)

getLength :: IO Int
getLength = do
 ss <- getLine
 pure $ length ss

data Car = Car
  { carName :: String
  , areAge  :: String
  } deriving Show

buildCar :: IO Car
buildCar =
  (fmap Car getLine) <*> getLine

{-
-}
