module FriTree where

import qualified  Data.Text as T

data FriTree = FriTree
  { ftSize :: FTSize
  , ftName :: FTName
  }

newtype FTSize = FTSize Int

newtype FTName = FTName T.Text

ftMapList :: (a -> b) -> [a] -> [b]
ftMapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : ftMapList functionA2B xs

ftFoldl :: (b -> a -> b) -> b -> [a] -> b
ftFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> ftFoldl functionBA2B (functionBA2B b x) xs

ftMapMaybe :: (a -> b) -> Maybe a -> Maybe b
ftMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

-- Enum
data FTColor
  = FTBlue
  | FTRed
  | FTGreen

ftColorToString :: FTColor -> String
ftColorToString color =
  case color of
    FTBlue  -> "Blue"
    FTRed   -> "Red"
    FTGreen -> "Green"

-- basic ADT (algebaic data type)
data FTVegetable
  = FTCelery
  | FTCarrot FTColor

ftVegetableToColor :: FTVegetable -> FTColor
ftVegetableToColor veg =
  case veg of
    FTCelery -> FTGreen
    FTCarrot col -> col

ftVegetableToString :: FTVegetable -> String
ftVegetableToString veg =
  ftColorToString $ ftVegetableToColor veg

-- polymorphic ADT (algebraic data type)
data FTKeep a
  = FTShoebox a
  | FTSafe a

redCarretInShoebox :: FTKeep FTVegetable
redCarretInShoebox = FTShoebox $ FTCarrot FTRed

carretInShoebox :: FTKeep (FTColor -> FTVegetable)
carretInShoebox = FTShoebox FTCarrot

celeryInSafe :: FTKeep FTVegetable
celeryInSafe = FTSafe FTCelery

data FTMaybe a
  = FTNothing
  | FTJust a

ftMapFTMaybe :: (a -> b) -> FTMaybe a -> FTMaybe b
ftMapFTMaybe funcationA2B ma =
  case ma of
    FTNothing -> FTNothing
    FTJust a -> FTJust $ funcationA2B a

instance Functor FTMaybe where
  fmap = ftMapFTMaybe

data FTList a
  = FTNothingList
  | FTList a (FTList a)

ftMapFTlist :: (a -> b) -> FTList a -> FTList b
ftMapFTlist functionaA2B listA =
  case listA of
    FTNothingList -> FTNothingList
    FTList x xs -> FTList (functionaA2B x) (ftMapFTlist functionaA2B xs)

instance Functor FTList where
  fmap = ftMapFTlist
