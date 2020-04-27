module Year2020.Month01.H20200128 () where

import qualified  Data.Text as T

-- Data type
data TueKey = TueKey
  { tkAmount :: TKAmount
  , tkName :: TKName
  }

-- New type
newtype TKAmount = TKAmount Int

newtype TKName = TKName T.Text

-- Recreate Mao for list
tkMapList :: (a -> b) -> [a] -> [b]
tkMapList funcationA2B listA=
  case listA of
    [] -> []
    (x:xs) -> funcationA2B x : tkMapList funcationA2B xs

-- Recreate foldl
tkFoldl :: (b -> a -> b) -> b -> [a] -> b
tkFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> tkFoldl functionBA2B (functionBA2B b x) xs

-- Recreate Map
tkMapMaybe :: (a -> b) -> Maybe a -> Maybe b
tkMapMaybe funcationA2B ma =
  case ma of
    Nothing -> Nothing
    Just x -> Just $ funcationA2B x

-- Create a Emun of colors
data TKColor
  = TKGreen
  | TKBlue
  | TKRed

-- Create a Color to a String
tkColorToString :: TKColor -> String
tkColorToString col =
  case col of
    TKGreen -> "Green"
    TKBlue -> "Blue"
    TKRed -> "Red"

-- Creat Basic ADT (Algebraic data type)
data TKVeg
  = TKCelory
  | TKCarret TKColor

-- Create Veg to Color
tkVegToColor :: TKVeg -> TKColor
tkVegToColor veg =
  case veg of
    TKCelory -> TKGreen
    TKCarret col -> col

-- Create Veg to String Color
tkVegToString :: TKVeg -> String
tkVegToString veg =
  tkColorToString $ tkVegToColor veg

-- Polymorphic ADT
data TkKeep a
  = TKShoebox a
  | TkSafe a

tkRedCarretInShoeBox :: TkKeep TKVeg
tkRedCarretInShoeBox =
  TKShoebox $ TKCarret TKRed

tkCarretInShoeBox :: TkKeep (TKColor -> TKVeg)
tkCarretInShoeBox =
  TKShoebox TKCarret

data TKMaybe a
  = TKNothing
  | TKJust a

tkMapTKMaybe :: (a -> b) -> TKMaybe a -> TKMaybe b
tkMapTKMaybe funcationA2B ma =
  case ma of
    TKNothing -> TKNothing
    TKJust a -> TKJust $ funcationA2B a

instance Functor TKMaybe where
  fmap = tkMapTKMaybe

data TKList a
  = TKNothingList
  | TKList a (TKList a)

tkMaoTKList :: (a -> b) -> TKList a -> TKList b
tkMaoTKList funcationA2B listA =
  case listA of
    TKNothingList -> TKNothingList
    TKList x xs -> TKList (funcationA2B x) (tkMaoTKList funcationA2B xs)

instance Functor TKList where
  fmap = tkMaoTKList

data CarInfo = CarInfo
  { carYear :: CarYear
  , carMake :: CarMake
  } deriving (Show)

newtype CarYear = CarYear Int deriving Show

newtype CarMake = CarMake T.Text deriving Show

getCarYear :: CarInfo -> CarYear
getCarYear carInfo =
  carYear carInfo

changeCarYear :: CarInfo -> CarYear -> CarInfo
changeCarYear carInfo newCarYear =
  carInfo { carYear =  newCarYear}

changeCarMake :: CarInfo -> CarMake -> CarInfo
changeCarMake carInfo newCarMake =
  carInfo { carMake = newCarMake}

tempCarInfo :: CarInfo
tempCarInfo =
  CarInfo { carYear = CarYear 1919
          , carMake = CarMake $ T.pack "BigJim"
          }
