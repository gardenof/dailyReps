module Year2020.Month02.H20200204
  ()where

import qualified  Data.Text as T

data Adidas = Adidas
  { adidasName :: AdidasName
  , adidasAge  :: AdidasAge
  }

newtype AdidasAge = AdidasAge Int
newtype AdidasName = AdidasName T.Text

createAdidas :: AdidasAge -> AdidasName -> Adidas
createAdidas age name =
  Adidas { adidasName = name, adidasAge = age}

templetAdidasAge :: AdidasAge
templetAdidasAge =
  AdidasAge 21

templetAdidasName :: AdidasName
templetAdidasName =
  AdidasName $ T.pack"Jimmy"

editAdidasAge :: Adidas -> AdidasAge -> Adidas
editAdidasAge record age =
  record { adidasAge = age}

editAdidasName :: Adidas -> AdidasName -> Adidas
editAdidasName record name =
  record { adidasName = name }

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just x  -> Just $ functionA2B x

foldlfoldl :: (b -> a -> b) -> b -> [a] -> b
foldlfoldl funcitonBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> foldlfoldl funcitonBA2B (funcitonBA2B b x) xs

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

insideKeep :: Keep a -> a
insideKeep keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarretInShoebox :: Keep Vegetable
redCarretInShoebox =
  Shoebox $ Carrot Red

carretInShoebox :: Keep (Color -> Vegetable)
carretInShoebox =
  Shoebox Carrot

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
