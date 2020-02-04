module H20200125 () where

import qualified  Data.Text as T

data SatBook = SatBook
  { bookTitle :: BookTitle
  , bookPages :: BookPages
  }

newtype BookTitle = BookTitle T.Text

newtype BookPages = BookPages Int

sbMapList :: (a -> b) -> [a] -> [b]
sbMapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : sbMapList functionA2B xs

sbFoldl :: (b -> a -> b) -> b -> [a] -> b
sbFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> sbFoldl functionBA2B (functionBA2B b x) xs

sbMapMaybe :: (a -> b) -> Maybe a -> Maybe b
sbMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

--Enums
data SBColor
  = SBBlue
  | SBRed
  | SBGreen

sbColorToString :: SBColor -> String
sbColorToString color =
  case color of
    SBBlue   -> "BLUE"
    SBRed    -> "RED"
    SBGreen  -> "GREEN"

-- baisc    ADT (algabraic data type)
data SBVegetable
  = SBCelery
  | SBCarret SBColor

sbVegToColor :: SBVegetable -> SBColor
sbVegToColor veg =
  case veg of
    SBCelery -> SBGreen
    SBCarret col -> col

sbVegToString :: SBVegetable -> String
sbVegToString veg =
  sbColorToString $ sbVegToColor veg

-- polymorphic ADT (algabraic data type)
data SBKeep a
  = SBShoebox a
  | SBSafe a

sbRedCarretInShoebox :: SBKeep SBVegetable
sbRedCarretInShoebox = SBShoebox $ SBCarret SBRed

sbCarretInShoebox :: SBKeep (SBColor -> SBVegetable)
sbCarretInShoebox = SBShoebox SBCarret

data SBMaybe a
  = SBNothing
  | SBJust a

sbMapSBMaybe :: (a -> b) -> SBMaybe a -> SBMaybe b
sbMapSBMaybe funcationA2B ma =
  case ma of
    SBNothing -> SBNothing
    SBJust a -> SBJust $ funcationA2B a

instance Functor SBMaybe where
  fmap = sbMapSBMaybe

data SBList a
  = SBNothingList
  | SBList a (SBList a)

sbMapSBList :: (a -> b) -> SBList a -> SBList b
sbMapSBList funcationAB listA =
  case listA of
    SBNothingList -> SBNothingList
    SBList x xs -> SBList (funcationAB x) (sbMapSBList funcationAB xs)

instance Functor SBList where
  fmap = sbMapSBList
