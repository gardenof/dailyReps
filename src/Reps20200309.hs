module Reps20200309 () where

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe functionA2MaybeB ma =
  case ma of
    Nothing -> Nothing
    Just a -> functionA2MaybeB a

joinList :: [[a]] -> [a]
joinList listOFlist =
  case listOFlist of
    [] -> []
    (list:restofLists) ->
      list <> joinList restofLists

bindlist :: (a -> [b]) -> [a] -> [b]
bindlist functionA2MaybeB listA =
  case listA of
    [] -> []
    (x:xs) ->
      functionA2MaybeB x <> bindlist functionA2MaybeB xs

bindListWithJoin :: (a -> [b]) -> [a] -> [b]
bindListWithJoin functionA2MaybeB listA =
  joinList $ fmap functionA2MaybeB listA

bindMaybeWithJoin :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybeWithJoin functionA2MaybeB ma =
  joinMaybe $ fmap functionA2MaybeB ma

joinListWithBind :: [[a]] -> [a]
joinListWithBind listOFlist =
  bindlist id listOFlist

joinMaybeWithBind :: Maybe (Maybe a) -> Maybe a
joinMaybeWithBind mMa =
  bindMaybe id mMa

{-
Write join for List
Write join for Maybe
Write Bind for List
Write Bind for Maybe

Write join for list useing Bind
Write join for Maybe useing Bind
Write Bind for list useing join
Write Bind for Maybe useing join
-}
