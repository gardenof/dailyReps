module Reps20200227 () where

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber maybeListOfString =
  fmap (fmap length) maybeListOfString

listMaybeString :: [Maybe String]
listMaybeString = [Just"asd",Just"asd"]

lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo listOfMaybeString =
  fmap (fmap length) listOfMaybeString

data Eden = Eden String Int Int

justString :: Maybe String
justString = Just "asd"

justInt :: Maybe Int
justInt = Just 55

createInstance :: Maybe Eden
createInstance =
  Eden <$> justString <*> justInt <*> justInt

createInstanceTwo :: Maybe Eden
createInstanceTwo =
  (pure Eden) <*> (pure "asd") <*> (pure 4) <*> (pure 4)

listString :: [String]
listString = ["asd","asd","asd"]

listInt :: [Int]
listInt = [1,2,3,4]

createInstanceThree :: [Eden]
createInstanceThree =
  (pure Eden) <*> (pure "sd") <*> (pure 34) <*> (pure 34)

functionComposition :: (b-> c) -> (a -> b) -> a -> c
functionComposition b2c a2b a =
  b2c (a2b a)

functionCompositionA :: (b-> c) -> (a -> b) -> (a -> c)
functionCompositionA b2c a2b =
  \a -> b2c (a2b a)

functionCompositionB :: (b-> c) -> ((a -> b) -> (a -> c))
functionCompositionB b2c =
  \a2b -> \a -> b2c (a2b a)

functionCompositionC :: ((b-> c) -> ((a -> b) -> (a -> c)))
functionCompositionC =
  \b2c -> \a2b -> \a -> b2c (a2b a)

functionCompositionD :: ((b-> c) -> (a -> b) -> a -> c)
functionCompositionD =
  \b2c a2b a -> b2c (a2b a)

getLength :: IO Int
getLength =
  fmap length getLine

data Car = Car
  { carName :: String
  , carAge :: String
  }

buildCar :: IO Car
buildCar =
  (fmap Car getLine) <*> getLine

applyListZip :: [a -> b] -> [a] -> [b]
applyListZip listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyListZip restOfFuncs


newtype EdenList = EdenList [String] deriving (Applicative)

pureList :: a -> [a]
pureList a = [a]

instance Applicative EdenList where
  pure = pureList
  (<*>) = applyListZip


{-
Create applicative instance for MakeMaybe
and MakeList FOR pure and (<*>)

instance Applicative MakeList where
  pure =
  (<*>) =

Create a new type around List
Implement Applicative for it
Provide a pure and apply instance
Make pure be correct for applyZip

Understand "->" in types
figure out the Type for Fmap, Pure, (<*>)
figure out the method for Fmap, Pure, (<*>)
-}
