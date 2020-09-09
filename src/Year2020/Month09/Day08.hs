module Year2020.Month09.Day08 () where

import Data.List as L

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe func ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ func a

fmapList :: (a -> b) -> [a] -> [b]
fmapList func list =
  case list of
    []   -> []
    x:xs -> func x : fmapList func xs

foldLList :: (b -> a -> b) -> b -> [a] -> b
foldLList func b list =
  case list of
    [] -> b
    x:xs -> foldLList func (func b x) xs

class HasMagnitude a where
  magnitude :: a -> Integer

instance HasMagnitude Integer where
  magnitude = id

instance HasMagnitude Int where
  magnitude = toInteger

instance HasMagnitude () where
  magnitude = const 0

instance HasMagnitude (a,b) where
  magnitude = const 2

instance HasMagnitude (a,b,c) where
  magnitude = const 3

instance HasMagnitude (Maybe a) where
  magnitude = maybe 0 (const 1)

instance HasMagnitude [a] where
  magnitude = magnitude . length

data PonziScheme
  = Victim String
  | Fraudster String [PonziScheme]

ponziMagnitude :: PonziScheme -> Integer
ponziMagnitude scheme =
  case scheme of
    Victim _ -> 1
    Fraudster _ victims ->
      sum ( 1 : map ponziMagnitude victims)

instance HasMagnitude PonziScheme where
  magnitude = ponziMagnitude

nullMagnitude :: HasMagnitude a => a -> Bool
nullMagnitude a = magnitude a == 0

compareMagnitude :: HasMagnitude a => a -> a -> Ordering
compareMagnitude left right =
  compare (magnitude left) (magnitude right)

sortNonNull :: HasMagnitude a => [a] -> [a]
sortNonNull =
  L.sortBy compareMagnitude . filter (not . nullMagnitude)
