module Car where

import qualified  Data.Text as T

data Car = Car
  { nickName :: CarNickName
  , miles    :: CarMiles
  }

newtype CarNickName = CarNickName T.Text

newtype CarMiles = CarMiles Int

mapList :: (a -> b) -> [a] -> [b]
mapList funcationA2B listA =
  case listA of
    [] -> []
    (x:xs) -> funcationA2B x : mapList funcationA2B xs

mapListTwo :: (a -> b) -> [a] -> [b]
mapListTwo funcationA2B [] = []
mapListTwo funcationA2B (x:xs) = funcationA2B x : mapListTwo funcationA2B xs

makeFoldl :: (b -> a -> b) -> b -> [a] -> b
makeFoldl funcationBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makeFoldl funcationBA2B (funcationBA2B b x) xs

makeFoldlTwo :: (b -> a -> b) -> b -> [a] -> b
makeFoldlTwo functionBA2B b [] = b
makeFoldlTwo functionBA2B b (x:xs) =
  makeFoldlTwo functionBA2B (functionBA2B b x) xs
