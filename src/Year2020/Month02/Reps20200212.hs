module Year2020.Month02.Reps20200212
  () where

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    []     -> []
    (x:xs) ->
      functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ functionA2B a

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
    Celery -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableColor veg

data Keep a
  = Shoebox a
  | Safe a

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a  ->
      JustMaybe $ functionA2B a

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

pureMaybe :: a -> MakeMaybe a
pureMaybe aa = JustMaybe aa

pureList :: a -> MakeList a
pureList aa = MakeList aa NothingList

members :: MakeMaybe (MakeList String)
members  = JustMaybe (MakeList ("asdf") (MakeList "sdf" NothingList))

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuns listA =
  case listOfFuns of
    [] -> []
    (x:xs) ->
      fmap x listA <> applyList xs listA

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

--applyList :: [a -> b] -> [a] -> [b]
applyListTwo :: MakeList (a -> b) -> MakeList a -> MakeList b
applyListTwo maybeListOfFunctions listA =
  case maybeListOfFunctions of
    NothingList   -> NothingList
    MakeList func restOfFuns ->
     appendList  (fmap func listA) (applyListTwo restOfFuns listA)


applyZipList :: [a -> b] -> [a] -> [b]
applyZipList functionA2B listA =
  case functionA2B of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (a:restOfAs) ->
          func a : applyZipList restOfFuncs restOfAs

applyMaybe :: MakeMaybe (a -> b) -> MakeMaybe a -> MakeMaybe b
applyMaybe maFunctionA2B ma =
  case maFunctionA2B of
    NothingMaybe   -> NothingMaybe
    JustMaybe func ->
      case ma of
        NothingMaybe -> NothingMaybe
        JustMaybe a  ->
          JustMaybe $ func a

lengthOfEachMember :: MakeMaybe (MakeList String)
                   -> MakeMaybe (MakeList Int)
lengthOfEachMember maListOfString =
  case maListOfString of
    NothingMaybe    -> NothingMaybe
    JustMaybe listA ->
      JustMaybe $ fmap length listA

maybeStringS :: [Maybe String]
maybeStringS = [Just"qwe",Just"asdf",Just"sdf"]

lengthOfEachMemberTT :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMemberTT listOfMaybeString =
  mapMakeList (mapMakeMaybe length) listOfMaybeString

data Dean = Dean String

justString :: MakeMaybe String
justString = JustMaybe "asd"

creatDeanInstance :: MakeMaybe Dean
creatDeanInstance =
  fmap Dean justString

data DeanStrInt = DeanStrInt String Int

justInt :: MakeMaybe Int
justInt = JustMaybe 5

createDeanStrIntInstance :: MakeMaybe DeanStrInt
createDeanStrIntInstance =
  applyMaybe(fmap DeanStrInt justString) justInt

{-
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
-}
