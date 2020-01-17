module Phone where

import qualified  Data.Text as T

data Phone = Phone
  { nickName :: PhoneNickName
  , phoneType :: PhoneType
  }

newtype PhoneNickName = PhoneNickName T.Text

data PhoneType = Apple
               | Something

maplist :: (a -> b) -> [a] -> [b]
maplist functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : maplist functionA2B xs

makefoldl :: (b -> a -> b) -> b -> [a] -> b
makefoldl functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> makefoldl functionBA2B (functionBA2B b x) xs


