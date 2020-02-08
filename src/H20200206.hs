module H20200206
  () where

import qualified  Data.Text as T

data Dog = Dog
  { dogSize :: DogSize
  , dogName :: DogName
  }

newtype DogName = DogName T.Text
newtype DogSize = DogSize Int

createDog :: DogName -> DogSize -> Dog
createDog name size =
  Dog { dogSize = size
      , dogName = name
      }

editDogNem :: Dog -> DogName -> Dog
editDogNem dog name =
  dog { dogName = name }

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
applyList listFunc listA =
  case listFunc of
    [] -> []
    (func:funcendlist) ->
      fmap func listA <> applyList funcendlist listA

applyListTwo :: [a -> b] -> [a] -> [b]
applyListTwo [] _ = []
applyListTwo (func:funcendlist) listA =
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

listMayStrToInt :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
listMayStrToInt listMayStr =
  mapMakeList (mapMakeMaybe length) listMayStr

listMayStrToIntTwo :: MakeList (MakeMaybe String)
                    -> MakeList (MakeMaybe Int)
listMayStrToIntTwo listMayStr =
  fmap (fmap length) listMayStr
