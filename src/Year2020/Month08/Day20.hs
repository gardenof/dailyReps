module Year2020.Month08.Day20 () where

-- Monad
-- PartA
joinList :: [[a]] -> [a]
joinList listOfList =
  case listOfList of
    [] -> []
    (x:xs) ->
      x <> joinList xs

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mma =
  case mma of
    Nothing -> Nothing
    Just ma -> ma

bindListFlip :: (a -> [b]) -> [a] -> [b]
bindListFlip func listA =
  case listA of
    []     -> []
    (x:xs) ->
      func x <> bindListFlip func xs

bindList :: [a] -> (a->[b]) -> [b]
bindList listA func =
  case listA of
    [] -> []
    (x:xs) -> func x <> bindList xs func

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinListWithBind :: [[a]] -> [a]
joinListWithBind list =
  bindList list id

joinMaybeWithBind :: Maybe(Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe mMa id

bindListWithJoin :: [a] -> (a -> [b]) -> [b]
bindListWithJoin listA func =
  joinList $ fmap func listA

