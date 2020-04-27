module Year2020.Month02.H20200208 () where

import qualified  Data.Text as T

data Moomoo = Moomoo
  { mooName :: MooName
  , mooAge  :: MooAge
  }

newtype MooAge  = MooAge Int
newtype MooName = MooName T.Text

tempMoo :: MooAge -> MooName -> Moomoo
tempMoo age name =
  Moomoo { mooName = name
         , mooAge  = age
         }

editMoo :: Moomoo -> MooName -> Moomoo
editMoo moo name =
  moo { mooName = name }

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe function ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ function a

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    []     -> b
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
    Celery     -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableColor veg

data Keep a
  = ShoeBox a
  | Safe a

takeOutKepp :: Keep a -> a
takeOutKepp keep =
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

fmapMakeMap :: (a -> b) -> MakeMaybe a -> MakeMaybe b
fmapMakeMap functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a  -> JustMaybe $ functionA2B a

instance Functor MakeMaybe where
  fmap = fmapMakeMap

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

fmapMakeList :: (a -> b) -> MakeList a -> MakeList b
fmapMakeList functionA2B listA =
  case listA of
    NothingList   -> NothingList
    MakeList x xs ->
      MakeList (functionA2B x) (fmapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = fmapMakeList

pureMaybe :: a -> Maybe a
pureMaybe a =
  Just a

pureList :: a -> [a]
pureList a =
  [a]

members :: Maybe [String]
members =
  Just ["someText"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunc ma =
  case maFunc of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ func a

applyList :: [a -> b] -> [a] -> [b]
applyList listFuncs listA =
  case listFuncs of
    [] -> []
    (func:funcS) ->
      fmap func listA <> applyList funcS listA

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber maListString =
  fmap (fmap length) maListString

someFunc :: [Maybe String]
someFunc = [Just"someText", Just"text", Just"textagin"]

somFunTwo :: [Maybe String] -> [Maybe Int]
somFunTwo listMaString =
  fmap (fmap length) listMaString
