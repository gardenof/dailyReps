module H20200131 () where

import qualified  Data.Text as T

data FriBoot = FriBoot
  { fbName :: FriBootName
  , fbAge  :: FriBootAge
  }

newtype FriBootAge = FriBootAge Int

newtype FriBootName = FriBootName T.Text

mapForList :: (a -> b) -> [a] -> [b]
mapForList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : mapForList functionA2B xs

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makefoldl functionBA2B (functionBA2B b x) xs

makeMapMaybe :: (a -> b) -> Maybe a -> Maybe b
makeMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

--Enum
data FbColor
  = FbGreen
  | FbBlue
  | FbRed

fbColorToString :: FbColor -> String
fbColorToString col =
  case col of
    FbGreen -> "Green"
    FbBlue  -> "Blue"
    FbRed   -> "Red"

-- basic ADT
data FbVeg
  = FbCelory
  | FbCarret FbColor

fbVegToCol :: FbVeg -> FbColor
fbVegToCol veg =
  case veg of
    FbCelory -> FbGreen
    FbCarret col -> col

fbVegToString :: FbVeg -> String
fbVegToString veg =
  fbColorToString $ fbVegToCol veg

-- Polymorphic ADT
data FbKeep a
  = FbShowbox a
  | FbSafe a

redCarretInShowbox :: FbKeep FbVeg
redCarretInShowbox = FbShowbox $ FbCarret FbRed

carretInShowbox :: FbKeep (FbColor -> FbVeg)
carretInShowbox = FbShowbox FbCarret

data MakeMaybe a
  = NothingMaybe
  | JustBe a

makeMapForMaybe :: (a ->b) -> MakeMaybe a -> MakeMaybe b
makeMapForMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustBe a -> JustBe $ functionA2B a

instance Functor MakeMaybe where
  fmap = makeMapForMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionBA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs-> MakeList (functionBA2B x) (mapMakeList functionBA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

-- -- -- -- NEW NEW

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe mFunctionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a ->
      case mFunctionA2B of
        Nothing -> Nothing
        Just function -> Just $ function a

applyMaybeV2 :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybeV2 mFunctionA2B ma =
  case mFunctionA2B of
    nothing -> Nothing
    Just function ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ function a
-- I do not understadn the diff yet but I
-- know there is, just need to look at it more.
--
-- Users/gardenof/Documents/dev/dailyReps/src/FriBoot.hs:114:5:
--- warning: [-Woverlapping-patterns]
--    Pattern match is redundant
--    In a case alternative: Just function -> ...
--    |
--114 |     Just function ->
--

purMaybe :: a -> Maybe a
purMaybe a =
  Just a

pureList :: a -> [a]
pureList a =
  [a]

zipList :: [a -> b] -> [a] -> [b]
zipList listOfFunctions listA =
  case listOfFunctions of
    [] -> []
    (function:functionList) ->
      case listA of
        [] -> []
        (x:xs) ->
          function x : zipList functionList xs


-- NEw new Two
--
member :: Maybe [String]
member = Just ["something"]

lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber maListString =
  case maListString of
    Nothing -> Nothing
    Just listA ->
      Just $ fmap length listA

somthingOne :: [Maybe String]
somthingOne =
  [Just "String"]

somethingTwo :: [Maybe String] -> [Maybe Int]
somethingTwo listMaString =
  case listMaString of
    []     -> []
    (x:xs) ->
      case x of
        Nothing     -> Nothing : somethingTwo xs
        Just string -> (Just $ length string) : somethingTwo xs
