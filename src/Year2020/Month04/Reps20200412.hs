module Year2020.Month04.Reps20200412 () where

{-
let
  name = expression
in
  expression using name

express must give a value
Must be valid when () are around it
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

tenV2 :: Int
tenV2 =
  let
    five = 5
    two = 2
  in
    five * two

eightLamada :: Int
eightLamada =
  (\lamEight -> lamEight*1)8

tenLame :: Int
tenLame =
  (\two ->5*two)2

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

infiOneAndZeros :: [Int]
infiOneAndZeros =
  let
    oneAndZeros = 1:zerosAndOnes
    zerosAndOnes = 0:oneAndZeros
  in
    oneAndZeros

oddTen :: Maybe Int
oddTen = do
  one <- Just 1
  four <- Just 4
  let
    two = 2
    three = 3
  Just (one+two+three+four)
