module H20200205 (applyList) where

import qualified  Data.Text as T

data Pizza = Pizza
  { pizzaSize :: PizzaSize
  , pizzaName :: PizzaName
  }

newtype PizzaName = PizzaName T.Text
newtype PizzaSize = PizzaSize Int

createPizza :: PizzaName -> PizzaSize -> Pizza
createPizza name size =
  Pizza { pizzaSize = size
        , pizzaName = name
        }

editPizzaNem :: Pizza -> PizzaName -> Pizza
editPizzaNem pizza name =
  pizza { pizzaName = name }

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

foldlfoldl :: (b -> a ->b) -> b -> [a] -> b
foldlfoldl funcationBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> foldlfoldl funcationBA2B (funcationBA2B b x) xs

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

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox = Shoebox $ Carrot Red

carrotInShoebox :: Keep (Color -> Vegetable)
carrotInShoebox = Shoebox Carrot

data MakeMaybe a
  = NothingMaybe
  | JustSomething a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustSomething a -> JustSomething $ functionA2B a

instance Functor MakeMaybe where
  fmap = mapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs -> MakeList (functionA2B x) (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

applMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applMaybe maybeFunc ma =
  case maybeFunc of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a ->
          Just $ func a

applyList :: [a -> b] -> [a] -> [b]
applyList listFunc listA =
  case listFunc of
    [] -> []
    (func:funcendlist) ->
      fmap func listA <> applyList funcendlist listA

pureMaybe :: a -> Maybe a
pureMaybe anA =
  Just anA

pureList :: a -> [a]
pureList anA =
  [anA]

members :: Maybe [String]
members =
  Just ["SomeLetters", "somethingtwo"]

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber mListofS =
  case mListofS of
    Nothing      -> Nothing
    Just listStr ->
      case listStr of
        []     -> Just []
        list ->
          Just $ fmap length list

createListOfMaybeStr :: [Maybe String]
createListOfMaybeStr =
  [Just "something", Just "somethingteo"]

listMayStrToInt :: [Maybe String] -> [Maybe Int]
listMayStrToInt listMayStr =
  case listMayStr of
    [] -> []
    (Nothing : mxs) -> [Nothing] <> listMayStrToInt mxs
    (Just x  : mxs) -> [Just (length x)] <> listMayStrToInt mxs

listMayStrToIntTwo :: [MakeMaybe String] -> [MakeMaybe Int]
listMayStrToIntTwo listMayStr =
  case listMayStr of
    (NothingMaybe : xms) ->
      listMayStrToIntTwo xms
    (JustSomething x : xms) ->
      [JustSomething (length x)] <> listMayStrToIntTwo xms
-- using funcs you already created , Don't use case
-- mapMakeList, mapMakeMaybe
-- Then a version with fmap

--USE YOUR OWN everything
-- Only use
