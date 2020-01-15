module Car where

import qualified  Data.Text as T

data Car = Car
  { nickName :: CarNickName
  , miles    :: CarMiles
  }

newtype CarNickName = CarNickName T.Text

newtype CarMiles = CarMiles Int
