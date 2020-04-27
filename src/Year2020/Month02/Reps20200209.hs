module Year2020.Month02.Reps20200209 () where

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ functionA2B a

makeFoldl :: (b -> a -> b) -> b -> [a] -> b
makeFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makeFoldl functionBA2B (functionBA2B b x) xs

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

vegetableToColor :: Vegetable -> Color
vegetableToColor veg =
  case veg of
    Celery -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableToColor veg

data Keep a
  = ShoeBox a
  | Safe a

takeOutKeep :: Keep a -> a
takeOutKeep keep =
  case keep of
    ShoeBox a -> a
    Safe a -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox =
  ShoeBox $ Carrot Red

carrotInShoebox :: Keep (Color -> Vegetable)
carrotInShoebox =
  ShoeBox Carrot

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
    MakeList x xs -> MakeList (functionA2B x) (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList


pureMaybe :: a -> Maybe a
pureMaybe aa=
  Just aa

pureList :: a -> [a]
pureList aa =
  [aa]

members :: Maybe [String]
members =
  Just ["some", "strings", "here"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeFunction ma =
  case maybeFunction of
    Nothing   -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a  ->
          Just $ func a

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:funcS) ->
      fmap func listA <> applyList funcS listA

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber maybeListStr =
  fmap (fmap length) maybeListStr


listOfMayBeStrings :: [Maybe String]
listOfMayBeStrings = [Just"some",Just"text",Just"here"]

lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo listOfMaybeString =
  fmap (fmap length) listOfMaybeString
