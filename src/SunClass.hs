module SunClass where

import qualified  Data.Text as T

data SunClass = SunClass
  { sunClassName :: SunClassName
  , sunClassLength :: SunClassLength
  }

newtype SunClassName = SunClassName T.Text

newtype SunClassLength = SunClassLength Int

scMapList :: (a -> b) -> [a] -> [b]
scMapList funcationA2B listA =
  case listA of
    [] -> []
    (x:xs) -> funcationA2B x : scMapList funcationA2B xs

scFoldl :: (b -> a -> b) -> b -> [a] -> b
scFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> scFoldl functionBA2B (functionBA2B b x) xs

scMapMaybe :: (a -> b) -> Maybe a -> Maybe b
scMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a


--Enum
data SCColor
  = SCBlue
  | SCRed
  | SCGreen

scColorToSctring :: SCColor -> String
scColorToSctring col =
  case col of
    SCBlue -> "BLUE"
    SCRed -> "RED"
    SCGreen -> "GREEN"

-- basic ADT (algabraic data type)
data SCVeg
  = SCCelory
  | SCCarret SCColor

scVegToColor :: SCVeg -> SCColor
scVegToColor veg =
  case veg of
    SCCelory -> SCGreen
    SCCarret col -> col

scVegToColorString :: SCVeg -> String
scVegToColorString veg =
  scColorToSctring $ scVegToColor veg

-- polymorfic ADT
data SCKeep a
  = SCShoebox a
  | SCSafe a

scRedCarretInShoeBox :: SCKeep SCVeg
scRedCarretInShoeBox = SCShoebox $ SCCarret SCRed

scCarretInShoeBox :: SCKeep (SCColor -> SCVeg)
scCarretInShoeBox = SCShoebox SCCarret

data SCMaybe a
  = SCNothing
  | SCJust a

scMapSCMaybe :: (a -> b) -> SCMaybe a -> SCMaybe b
scMapSCMaybe functionA2B scma =
  case scma of
    SCNothing -> SCNothing
    SCJust a -> SCJust $ functionA2B a

instance Functor SCMaybe where
  fmap = scMapSCMaybe


data SCList a
  = SCNothingList
  | SCList a (SCList a)

scMapSCList :: (a -> b) -> SCList a -> SCList b
scMapSCList functionA2B listA =
  case listA of
    SCNothingList -> SCNothingList
    SCList x xs -> SCList (functionA2B x) (scMapSCList functionA2B xs)

instance Functor SCList where
  fmap = scMapSCList

