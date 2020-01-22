module Spoon where

import qualified  Data.Text as T

data Spoon = Spoon
  { spoonType   :: SpoonType
  , spoonAmount :: SpoonAmount
  }

newtype SpoonAmount = SpoonAmount Int

newtype SpoonType = SpoonType T.Text

makeMapList :: (a -> b) -> [a] -> [b]
makeMapList funcationA2B listA =
  case listA of
    [] -> []
    (x:xs) -> funcationA2B x : makeMapList funcationA2B xs

makeFoldl :: (b -> a -> b) -> b -> [a] -> b
makeFoldl functionbA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makeFoldl functionbA2B (functionbA2B b x) xs

data SpoonColor
  = RRed
  | GGreen
  | BBlue

colorToHexString :: SpoonColor -> String
colorToHexString color =
  case color of
    RRed   -> "RedHex"
    GGreen -> "GreenHex"
    BBlue  -> "BlueHex"

data Vegetable
  = Celery
  | Carrot SpoonColor

vegetableToColor :: Vegetable -> SpoonColor
vegetableToColor veg =
  case veg of
    Celery -> GGreen
    Carrot col -> col

vegetableToHexString :: Vegetable -> String
vegetableToHexString veg =
  colorToHexString $ vegetableToColor veg

data Keep a
  = ShoeBox a
  | Safe a

carrotBox :: Keep Vegetable
carrotBox = ShoeBox $ Carrot RRed

carrotBoxNone :: Keep (SpoonColor -> Vegetable)
carrotBoxNone = ShoeBox $ Carrot

