module Year2020.Month02.Reps20200213 () where

mapList :: (a -> b) -> [a] -> [b]
mapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2B x : mapList functionA2B xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe functionA2B ma =
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

takeOut :: Keep a -> a
takeOut keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox =
  Shoebox $ Carrot Red

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox =
  Shoebox Carrot

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
members = Just ["asd","asdf","sdf"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunc ma =
  case maFunc of
    Nothing   -> Nothing
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
      fmap func listA <> (applyList restOfFuncs listA)

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restOfFuncs xs

lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListOfStrings =
   mapMakeMaybe (mapMakeList length) maybeListOfStrings

listOfMaybeString :: MakeList (MakeMaybe String)
listOfMaybeString = MakeList (JustMaybe"asd") (MakeList (JustMaybe"asd") NothingList)

{-
* `~~~~` :: [Maybe String] -> [Maybe Int]
* write with your functions and then fmap BOTH

##### Pre applicative

* Create a data that is a String
* Create a :: Maybe String
* Use fmap to create an instance of that data type using the maybe value.
* Create a data type that needs a String and Int
pro * Craete a :: Maybe Int
* Use fmap and apply to create an instance of that data type (String Int) using the the two Maybe values

Create this
create as sdfsdf instances
 <>s
 psif

rep funtors laws

Use above
-}
