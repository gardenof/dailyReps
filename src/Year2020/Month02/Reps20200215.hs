module Year2020.Month02.Reps20200215 () where

mapList :: (a -> b) -> [a] -> [b]
mapList functionA2B listA =
  case listA of
    [] -> []
    (a:as) ->
      functionA2B a : mapList functionA2B as

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a ->
      Just $ functionA2B a

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    [] -> b
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

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a -> JustMaybe $ functionA2B a

instance Functor MakeMaybe where
  fmap = mapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList functionA2B listA =
  case listA of
    NothingList -> NothingList
    MakeList x xs ->
      MakeList
        (functionA2B x)
        (mapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe aa = Just aa

pureList :: a -> [a]
pureList aa = [aa]

members :: Maybe [String]
members = Just ["asdf","sdf","adf"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunc ma =
  case maFunc of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ func a

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:restOfFuncs) ->
      fmap func listA <> applyList restOfFuncs listA

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList listOfFuncs listA =
  case listOfFuncs of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          func x : applyZipList restOfFuncs xs

--lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
lengthOfEachMeber :: MakeMaybe (MakeList String)
                  -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListString =
  mapMakeMaybe (mapMakeList length) maybeListString

listJustString :: [Maybe String]
listJustString = [Just"asd",Just"asd",Just"asd"]

--`~~~~` :: [Maybe String] -> [Maybe Int]
lengthOfEachMeberTwo :: MakeList (MakeMaybe String)
                     -> MakeList (MakeMaybe Int)
lengthOfEachMeberTwo listOfMaybeString =
  mapMakeList (mapMakeMaybe length) listOfMaybeString

data EdenString = EdenString String

justString :: Maybe String
justString = Just "asd"

createInstance :: Maybe EdenString
createInstance = fmap EdenString justString

data EdenStrInt = EdenStrInt String Int

justInt :: Maybe Int
justInt = Just 5

createInstanceTwo :: Maybe EdenStrInt
createInstanceTwo =
  applyMaybe (fmap EdenStrInt justString) justInt


{-
Functor Laws
Functors must preserve identity morphisms
fmap id = id

Functors preserve composition of morphisms
fmap (f . g)  ==  fmap f . fmap g


Methods
pure :: a -> f a
  Lift a value.

(<*>) :: f (a -> b) -> f a -> f b
  Sequential application.
  A few functors support an implementation of <*>
  that is more efficient than the default one.
-}
