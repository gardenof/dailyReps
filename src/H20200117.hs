module H20200117 () where

import qualified  Data.Text as T

data Phone = Phone
  { nickName :: PhoneNickName
  , phoneType :: PhoneType
  }

newtype PhoneNickName = PhoneNickName T.Text

data PhoneType = Apple
               | Something

maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : maplist functionA2B xs

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makefoldl functionBA2B (functionBA2B b x) xs

data Color
  = Red
  | Green
  | Blue

colorToHexSttring :: Color -> String
colorToHexSttring color =
  case color of
    Red   -> "#FF0303"
    Green -> "#3CFF03"
    Blue  -> "#0329FF"

data Vegetable
  = Celery
  | Carrot Color

vegetableToColor :: Vegetable -> Color
vegetableToColor veg =
  case veg of
    Celery -> Green
    Carrot color -> color

vegToHexColor :: Vegetable -> String
vegToHexColor veg =
  case veg of
    Celery -> colorToHexSttring Green
    Carrot color -> colorToHexSttring color

vegToHexColorTwo :: Vegetable -> String
vegToHexColorTwo veg =
  colorToHexSttring $ vegetableToColor veg

data Keep a
  = SecureVault a
  | ShoeBox a

takeout :: Keep a -> a
takeout k =
  case k of
    SecureVault b -> b
    ShoeBox c -> c

safeCarrot :: Keep Vegetable
safeCarrot = ShoeBox $ Carrot Red

foo :: Keep (Color -> Vegetable)
foo = ShoeBox Carrot


