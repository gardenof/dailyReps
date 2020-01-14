module Tree where

import qualified  Data.Text as T

data Tree = Tree
  { nickName = NickName
  , age      = TreeAge
  }

newtype Age = Age T.Text

newtype TreeAge = TreeAge Int


maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
 case listA of
   [] ->
   (x:xs) -> functionA2B x : maplist xs
