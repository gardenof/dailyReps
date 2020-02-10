module Reps20200210 () where

-- Talk with David
--888888888888888888888888888888888888888888888--
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunction ma =
  case maFunction of
    Nothing -> Nothing
    Just func  ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ func a


applyList :: [a -> b] -> [a] -> [b]
applyList listofFunc listA =
  case listofFunc of
    [] -> []
    (fun:funS) ->
          fmap fun listA <> applyList funS listA


data Eden = Eden String

edenmaybe :: Maybe String
edenmaybe = Just "something"

anEden :: Maybe Eden
anEden =
  fmap Eden edenmaybe

data Jim = Jim String Int

maybeInt :: Maybe Int
maybeInt = Just 5

anJim :: Maybe Jim
anJim =  applyMaybe (fmap Jim edenmaybe) maybeInt
--888888888888888888888888888888888888888888888--


fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    []     -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ functionA2B a

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl funcitonBA2B b listA =
  case listA of
    []     -> b
    (x:xs) ->
      makefoldl funcitonBA2B (funcitonBA2B b x) xs

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
    Celery     -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableColor veg

data Keep a
  = Shoebox a
  | Safe a

takeOutKeep :: Keep a -> a
takeOutKeep keep =
  case keep of
    Shoebox a -> a
    Safe a    -> a

redCarretInShoebox :: Keep Vegetable
redCarretInShoebox =
  Shoebox $ Carrot Red

carretInShoebox :: Keep (Color -> Vegetable)
carretInShoebox = Shoebox Carrot

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

fmapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
fmapMakeMaybe functionA2B ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a  -> JustMaybe $ functionA2B a

instance Functor MakeMaybe where
  fmap = fmapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

fmapMakeList :: (a -> b) -> MakeList a -> MakeList b
fmapMakeList functionA2B listA =
  case listA of
    NothingList   -> NothingList
    MakeList x xs ->
      MakeList (functionA2B x) (fmapMakeList functionA2B xs)

instance Functor MakeList where
  fmap = fmapMakeList

pureMaybe :: a -> Maybe a
pureMaybe aa = Just aa

pureList :: a -> [a]
pureList aa = [aa]

members :: Maybe [String]
members = Just ["wer","wer","sdf"]

applyMaybeT :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybeT maybeFunc ma =
  case maybeFunc of
    Nothing   -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a  ->
          Just $ func a

applyListT :: [a -> b] -> [a] -> [b]
applyListT listOfFuncs listA =
  case listOfFuncs of
    []           -> []
    (func:funcs) ->
      fmap func listA <> applyListT funcs listA

applyZipList :: [a -> b] -> [a] -> [b]
applyZipList listOfFuncs listA =
  case listOfFuncs of
    []                 -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:xs) ->
          [func x] <> applyZipList restOfFuncs xs

lengthOfEachMeber :: MakeMaybe (MakeList String) -> MakeMaybe (MakeList Int)
lengthOfEachMeber maybeListString =
  fmapMakeMaybe (fmapMakeList length) maybeListString

lengthOfEachMeberTwo :: MakeMaybe (MakeList String) -> MakeMaybe (MakeList Int)
lengthOfEachMeberTwo maybeListString =
  fmap (fmap length) maybeListString

listOfMaybeStrings :: [Maybe String]
listOfMaybeStrings = [Just"sdf",Just"dfw",Just"ee"]

lengthOfEachMeberThree :: MakeList (MakeMaybe String)
                       -> MakeList (MakeMaybe Int)
lengthOfEachMeberThree listOfMaybeStrings =
  fmapMakeList (fmapMakeMaybe length) listOfMaybeStrings

lengthOfEachMeberFour :: MakeList (MakeMaybe String)
                       -> MakeList (MakeMaybe Int)
lengthOfEachMeberFour listOfMaybeStrings =
  fmap (fmap length) listOfMaybeStrings

data Name = Name String

justString :: Maybe String
justString = Just "gsdfg"

fmapCreatName :: Maybe Name
fmapCreatName =
  fmap Name justString

data CustomSize = CustomSize String Int

justInt :: Maybe Int
justInt = Just 34

fmapCreatCustomSize :: Maybe CustomSize
fmapCreatCustomSize =
  applyMaybeT (fmap CustomSize justString) justInt

data Dean = Dean Int String Int

maybeDean :: Maybe Dean
maybeDean =
  -- fmap and applemaybeT
  applyMaybeT (applyMaybeT (fmap Dean justInt) justString) justInt
