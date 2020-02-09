module H20200207 where

import qualified  Data.Text as T

data Cat = Cat
  { catName :: CatName
  , catAge  :: CatAge
  }

newtype CatAge = CatAge Int
newtype CatName = CatName T.Text

creatACat :: CatName -> CatAge -> Cat
creatACat name age =
  Cat { catName = name
      , catAge  = age
      }

editCatName :: Cat -> CatName -> Cat
editCatName cat name =
  cat { catName = name}

fmapList :: (a -> b) -> [a] -> [b]
fmapList funcationA2B listA =
  case listA of
    []     -> []
    (x:xs) ->
      funcationA2B x : fmapList funcationA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ functionA2B a

foldlfoldl :: (b -> a -> b) -> b -> [a] -> b
foldlfoldl funcitonBA2B b listA =
  case listA of
    []     -> b
    (x:xs) ->
      foldlfoldl funcitonBA2B (funcitonBA2B b x) xs

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
    Celery     -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableToColor veg

data Keep a
  = Shoebox a
  | Safe a

takeout :: Keep a -> a
takeout keep =
  case keep of
    Shoebox a -> a
    Safe a    -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox =
  Shoebox $ Carrot Red

carrotInShoebox :: Keep (Color -> Vegetable)
carrotInShoebox =
  Shoebox Carrot

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a  -> JustMaybe $ functionA2B a

instance Functor MakeMaybe where
  fmap = mapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionA2B listA =
  case listA of
    NothingList   -> NothingList
    MakeList x xs -> MakeList (functionA2B x) (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

applyMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMaybe maFunction ma =
  case maFunction of
    NothingMaybe   -> NothingMaybe
    JustMaybe func ->
      case ma of
        NothingMaybe -> NothingMaybe
        JustMaybe a  ->
          JustMaybe $ func a

pureMaybe :: a -> Maybe a
pureMaybe a =
  Just a

pureList :: a -> [a]
pureList a =
  [a]

member :: Maybe [String]
member =
  Just ["someText"]

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber mayListString =
  fmap (fmap length) mayListString

someFunction :: [Maybe String]
someFunction =
  [Just "someText"]

someFunctionTwo :: [Maybe String] -> [Maybe Int]
someFunctionTwo listMaString =
  fmap (fmap length) listMaString
