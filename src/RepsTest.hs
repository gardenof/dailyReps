module RepsTest () where

data MakeList a
  = NothingList
  | MakeList a (MakeList a)
  deriving (Show)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs -> MakeList (functionA2B x) (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuns listA =
  case listOfFuns of
    [] -> []
    (x:xs) ->
      fmap x listA <> applyList xs listA

--applyList :: [a -> b] -> [a] -> [b]
applyListTwo :: MakeList (a -> b) -> MakeList a -> MakeList (MakeList b)
applyListTwo maybeListOfFunctions listA =
  case maybeListOfFunctions of
    NothingList   -> NothingList
    MakeList func restOfFuns ->
      MakeList (fmap func listA) (applyListTwo restOfFuns listA)

{-
joinList :: MakeList (MakeList b) -> MakeList b
joinList listB =
  case listB of
    NothingList -> NothingList
    MakeList b ->
-}

--Wronge because if nd list nothing lose 2st list
zipLists :: MakeList a -> MakeList a -> MakeList a
zipLists listOne listTwo =
  case listOne of
    NothingList   -> NothingList
    MakeList x xs ->
      case listTwo of
        NothingList -> NothingList
        MakeList x2 xs2 ->
          MakeList x (MakeList x2 (appendList xs xs2))

appendList :: MakeList a -> MakeList a -> MakeList a
appendList listOne listTwo =
  case listOne of
    MakeList x xs ->
      MakeList x (appendList xs listTwo)
    NothingList ->
      case listTwo of
        NothingList -> NothingList
        MakeList x2 xs2 ->
          MakeList x2 (appendList xs2 NothingList)

appendListTwo :: MakeList a -> MakeList a -> MakeList a
appendListTwo (MakeList x xs) listTwo =
      MakeList x (appendList xs listTwo)
appendListTwo NothingList (MakeList x2 xs2) =
      MakeList x2 (appendList xs2 NothingList)
appendListTwo NothingList NothingList =
      NothingList
