module Reps20200215 () where

mapList :: (a -> b) -> [a] -> [b]
mapList functionA2B listA =
  case listA of
    [] -> []
    (a:as) ->
      functionA2B a : mapList functionA2B as

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a ->
      Just $ functionA2B a

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) ->
      makefoldl functionBA2B (functionBA2B b x) xs

data Color
  = Green
  | Blue
  | Red

colorToString :: Color -> String
colorToString col =
  case col of
    Green -> "Green"
    Blue  -> "Blue"
    Red   -> "Red"

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

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

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
      MakeList
        (functionA2B x)
        (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe aa = Just aa

pureList :: a -> [a]
pureList aa = [aa]

members :: Maybe [String]
members = Just ["asdf","sdf","adf"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunc ma =
  case maFunc of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ func a

{- ## Reps
* applyList ::

* applyZipList ::

* lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
  * Should work like
  * Just ["Bob", "Carol"] -> Just [3,5]
* write with your functions and then fmap BOTH

* `~~~~` :: [Maybe String]

* `~~~~` :: [Maybe String] -> [Maybe Int]
* write with your functions and then fmap BOTH

##### Pre applicative

* Create a data that is a String
* Create a :: Maybe String
* Use fmap to create an instance of that data type using the maybe value.
* Create a data type that needs a String and Int
* Craete a :: Maybe Int
* Use fmap and apply to create an instance of that data type (String Int) using the the two Maybe values

-}
