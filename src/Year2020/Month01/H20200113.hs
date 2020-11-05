module Year2020.Month01.H20200113 () where

maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : maplist functionA2B xs
