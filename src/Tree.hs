module Tree where

import qualified  Data.Text as T

data Tree = Tree
  { nickName :: NickName
  , age      :: TreeAge
  }

newtype NickName = NickName T.Text

newtype TreeAge = TreeAge Int


maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
 case listA of
   []     -> []
   (x:xs) -> functionA2B x : maplist functionA2B xs

makeFoldl :: (b -> a -> b) -> b -> [a] -> b
makeFoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makeFoldl functionBA2B (functionBA2B b x) xs

make2Foldl :: (b -> a -> b) -> b -> [a] -> b
make2Foldl funcitonBA2B b [] = b
make2Foldl funcitonBA2B b (x:xs) = make2Foldl funcitonBA2B (funcitonBA2B b x) xs
