module Year2020.Month03.Reps20200316 () where

joinList :: [[a]] -> [a]
joinList listOfLists =
  case listOfLists of
    []  ->[]
    (list:restOfLists) ->
      list <> (joinList restOfLists)

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindList :: [a] -> (a -> [b]) -> [b]
bindList listA functionA2ListB =
  case listA of
    []  -> []
    (x:xs) ->
      functionA2ListB x <> bindList xs functionA2ListB

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma functionA2ListB =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2ListB a

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOfLists =
  bindList listOfLists id

joinMaybeWithBind ::Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mma =
  bindMaybe mma id

bindListWithJoin :: [a] -> (a->[b]) -> [b]
bindListWithJoin listA functionA2ListB =
  joinList $ fmap functionA2ListB listA

bindMaybeWithJoin :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybeWithJoin ma functionA2ListB =
  joinMaybe $ fmap functionA2ListB ma























