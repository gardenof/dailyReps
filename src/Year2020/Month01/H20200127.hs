module Year2020.Month01.H20200127 () where

import qualified  Data.Text as T

data MonRunner = MonRunner
  { monRunnerAge :: MonRunnerAge
  , monRunnerName :: MonRunnerName
  }

newtype MonRunnerName = MonRunnerName Int

newtype MonRunnerAge = MonRunnerAge T.Text

mrMapList :: (a -> b) -> [a] -> [b]
mrMapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : mrMapList functionA2B xs

mrFoldl :: (b -> a -> b) -> b -> [a] -> b
mrFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> mrFoldl functionBA2B (functionBA2B b x) xs

mrMapMaybe :: (a -> b) -> Maybe a -> Maybe b
mrMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just x -> Just $ functionA2B x

--Enum
data MRColor
  = MRGreen
  | MRBlue
  | MRRed

mrColorToString :: MRColor -> String
mrColorToString col =
  case col of
    MRGreen -> "GREEN"
    MRBlue  -> "BLUE"
    MRRed   -> "RED"

-- basic ADT (algabraic data tyoe)
data MRVeg
  = MRCelery
  | MRCarret MRColor

mrVegToColor :: MRVeg -> MRColor
mrVegToColor veg =
 case veg of
  MRCelery -> MRGreen
  MRCarret col -> col

mrVegToString :: MRVeg -> String
mrVegToString veg =
  mrColorToString $ mrVegToColor veg

-- polymorphic ADT
data MRKeep a
  = MRShoebox a
  | MRSafe a

mrRedCarretInShoeBox :: MRKeep MRVeg
mrRedCarretInShoeBox = MRShoebox $ MRCarret MRRed

mrCarrrectInShoeBox :: MRKeep (MRColor -> MRVeg)
mrCarrrectInShoeBox = MRShoebox MRCarret


data MRMakeMaybe a
  = MRNothing
  | MRJust a

mrMapMRMaybe :: (a -> b) -> MRMakeMaybe a -> MRMakeMaybe b
mrMapMRMaybe functionA2B ma =
  case ma of
    MRNothing -> MRNothing
    MRJust a -> MRJust $ functionA2B a

instance Functor MRMakeMaybe where
  fmap = mrMapMRMaybe

data MakeList a
  = MRNothingList
  | MakeList a (MakeList a)

mrMapMakeList :: (a -> b) -> MakeList a -> MakeList b
mrMapMakeList functionA2B listA =
  case listA of
    MRNothingList -> MRNothingList
    MakeList x xs -> MakeList (functionA2B x) (mrMapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mrMapMakeList
