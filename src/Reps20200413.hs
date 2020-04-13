module Reps20200413 () where

{-
  let
    name = expression
  in
    expression using name

express must give a value
Must be vaild when () are around it
-}

eight :: Int
eight =
  let
    four = 4
  in
    four * 2

nine :: Int
nine =
  let
    three = 3
  in
    three * three

ten :: Int
ten =
  let
    five = 5
  in
    let
      two = 2
    in
      five * two

tenv2 :: Int
tenv2 =
  let
    five = 5
    two = 2
  in
    five * two

eightLamda :: Int
eightLamda =
  (\lamEight -> lamEight*1)8

tenLame :: Int
tenLame =
  (\two -> 5 * two) 2

calFiveV2 :: Int
calFiveV2 =
  let
    two = 2
    four = two * two
    one = 1
    five = four + one
  in
    five

listOf1000 :: [Int]
listOf1000 =
  let
    one = 1:one
  in
    take 1000 one

oddTen :: Maybe Int
oddTen = do
  one <- Just 1
  four <- Just 4
  let
    two = 2
    three = 3
  Just (one+two+three+four)
