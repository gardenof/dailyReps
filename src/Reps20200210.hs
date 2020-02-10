module Reps20200210 () where

-- Talk with David
--888888888888888888888888888888888888888888888--
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maFunction ma =
  case maFunction of
    Nothing -> Nothing
    Just func  ->
      case ma of
        Nothing -> Nothing
        Just a -> Just $ func a


applyList :: [a -> b] -> [a] -> [b]
applyList listofFunc listA =
  case listofFunc of
    [] -> []
    (fun:funS) ->
          fmap fun listA <> applyList funS listA


data Eden = Eden String

edenmaybe :: Maybe String
edenmaybe = Just "something"

anEden :: Maybe Eden
anEden =
  fmap Eden edenmaybe

data Jim = Jim String Int

maybeInt :: Maybe Int
maybeInt = Just 5

anJim :: Maybe Jim
anJim =  applyMaybe (fmap Jim edenmaybe) maybeInt
--888888888888888888888888888888888888888888888--
