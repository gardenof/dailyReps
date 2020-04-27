module Year2020.Month01.H20200113 () where

import qualified  Data.Text as T

data Bike = Bike { nickName :: NickName
                 , odometer :: Odometer
                 }

newtype NickName = NickName T.Text

newtype Odometer = Odometer Int

maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : maplist functionA2B xs
