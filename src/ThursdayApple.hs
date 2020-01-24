module ThursdayApple where

import qualified  Data.Text as T

data ThursdayApple = ThursdayApple
  { tAppleSize :: TAppleSize
  , tAppleType :: TAppleType
  }

newtype TAppleSize = TAppleSize Int
newtype TAppleType = TAppleType T.Text

tMakeMap4List :: (a -> b) -> [a] -> [b]
tMakeMap4List funcationA2B listA =
  case listA of
    [] -> []
    (x:xs) -> funcationA2B x : tMakeMap4List funcationA2B xs

tMakeFoldl :: (b -> a -> b) -> b -> [a] -> b
tMakeFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> tMakeFoldl functionBA2B (functionBA2B b x) xs

data TAColor
  = TARed
  | TABlue
  | TAGreen

taColorToString :: TAColor -> String
taColorToString taColor =
  case taColor of
    TARed   -> "Red"
    TABlue  -> "Blue"
    TAGreen -> "Green"

data TAVegetable
  = TACelery
  | TACarrot TAColor

taVegetableToColor :: TAVegetable -> TAColor
taVegetableToColor taVeg =
  case taVeg of
    TACelery -> TAGreen
    TACarrot color -> color

taVegetableToColorString :: TAVegetable -> String
taVegetableToColorString taVeg =
  taColorToString $ taVegetableToColor taVeg

data TAKeep a
  = TAShoeBox a
  | TASafe a

taTakeOutKeep :: TAKeep a -> a
taTakeOutKeep keep =
  case keep of
    TAShoeBox x -> x
    TASafe x -> x

taRedCarrotShoeBox :: TAKeep TAVegetable
taRedCarrotShoeBox = TAShoeBox $ TACarrot TARed

taCarrotShoeBox :: TAKeep (TAColor -> TAVegetable)
taCarrotShoeBox = TAShoeBox TACarrot

taCeleryShoeBox :: TAKeep TAVegetable
taCeleryShoeBox = TAShoeBox TACelery

taMapMaybe :: (a -> b) -> Maybe a -> Maybe b
taMapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

data MakeMaybe a
  = MakeNothing
  | MakeJust a

makeMapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
makeMapMakeMaybe funcationA2B ma =
  case ma of
    MakeNothing -> MakeNothing
    MakeJust a -> MakeJust $ funcationA2B a

instance Functor MakeMaybe where
  fmap = makeMapMakeMaybe

data MakeList a
  = MakeNothingList
  | MakeCons a (MakeList a)

data MakeListTwo a
  = MakeListTwoNothing
  | MakeListTwo a (MakeListTwo a)

fiveM :: MakeList Int
fiveM = MakeCons 5 (MakeNothingList)

--makeMapMakeList :: (a -> b) -> [a] -> [b]
makeMapMakeList :: (a -> b) -> MakeListTwo a -> MakeListTwo b
makeMapMakeList functionA2B  listA =
  case listA of
    MakeListTwoNothing -> MakeListTwoNothing
    MakeListTwo a as-> MakeListTwo (functionA2B a) (makeMapMakeList functionA2B as)

instance Functor MakeListTwo where
  fmap = makeMapMakeList





