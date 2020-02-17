module Reps20200217 () where

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

makeFoldL :: (b -> a -> b) -> b -> [a] -> b
makeFoldL functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) ->
      makeFoldL functionBA2B (functionBA2B b x) xs

data Color
  = Green
  | Blue
  | Red

colorToString :: Color -> String
colorToString col =
  case col of
    Green -> "Green"
    Blue -> "Blue"
    Red -> "Red"

data Vegetable
  = Celery
  | Carrot Color

vegetableColor :: Vegetable -> Color
vegetableColor veg =
  case veg of
    Celery -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableColor veg

data Keep a
  = Shoebox a
  | Safe a

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot

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

data Makelist a
  = NothingList
  | Makelist a (Makelist a)

mapMakeList :: (a -> b) -> Makelist a -> Makelist b
mapMakeList functionA2B lista =
  case lista of
    NothingList -> NothingList
    Makelist x xs ->
      Makelist (functionA2B x) (mapMakeList functionA2B xs)

instance Functor Makelist where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe aa = Just aa

pureList :: a -> [a]
pureList aa = [aa]

members :: Maybe [String]
members = Just ["sdf","sdf","sdf"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunctionA2B ma =
  case maybeFunctionA2B of
    Just func ->
      fmap func ma

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuncs listA =
  case listOfFuncs of
    [] ->[]
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
lengthOfEachMeber :: MakeMaybe (Makelist String)
                  -> MakeMaybe (Makelist Int)
lengthOfEachMeber maybeListOftring =
  mapMakeMaybe (mapMakeList length) maybeListOftring

listOfMaybeString :: [Maybe String]
listOfMaybeString = [Just"asd",Just"sdf",Just"sdf"]

lengthOfEachMeberTwo :: Makelist (MakeMaybe String)
                     -> Makelist (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeStringTwo =
  mapMakeList (mapMakeMaybe length) listOfMaybeStringTwo

data EdenString = EdenString String

justString :: Maybe String
justString = Just "sdf"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStringInt = EdenStringInt String Int

justInt :: Maybe Int
justInt = Just 5

createInstanceTwo :: Maybe EdenStringInt
createInstanceTwo =
  applyMaybe (fmap EdenStringInt justString) justInt

{-
famp id = id

famp (f . g) == fmap f . fmap g

-}

