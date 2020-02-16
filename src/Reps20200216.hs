module Reps20200216 () where

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

makeFoldl :: (b -> a -> b) -> b -> [a] -> b
makeFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) ->
      makeFoldl functionBA2B (functionBA2B b x) xs

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
redCarrotShoebox =
  Shoebox $ Carrot Red

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
members = Just ["df","sdf","df"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunc ma =
  case maybeFunc of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a ->
          Just $ func a

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
lengthOfEachMeber maybeListString =
  mapMakeMaybe (mapMakeList length) maybeListString

listOfMaybeSring :: [Maybe String]
listOfMaybeSring = [Just"sdf",Just"sdf",Just"sfg"]

--lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo  listOfMaybeString =
  mapMakeList (mapMakeMaybe length) listOfMaybeString

data EdenString = EdenString String

justString :: Maybe String
justString = Just "ASd"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStrInt = EdenStrInt String Int

justInt :: Maybe Int
justInt = Just 5

createInstanceTwo :: Maybe EdenStrInt
createInstanceTwo =
  applyMaybe(fmap EdenStrInt justString) justInt
