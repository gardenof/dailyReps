module Year2020.Month07.Day28 () where

{-
  let
    name = expression
  in
    expression
-}

eight :: Int
eight =
  let four = 4
  in four * 2

nine :: Int
nine =
  let three = 3
   in three * three

longTen :: Int
longTen =
  let five = 5
   in let two = 2
       in five * two

shortTen :: Int
shortTen =
  let five = 5
      two  = 2
  in five * two

lambdaTen :: Int
lambdaTen =
 (\five two -> five * two) 5 2

orderedFive :: Int
orderedFive =
  let
    five = four + one
    four = two * two
    two  = 2
    one  = 1
  in
    five

thousandOnes :: [Int]
thousandOnes =
  let ones = 1 : ones
  in take 1000 ones

justTen :: Maybe Int
justTen = do
  one  <- Just 1
  let two   = 2
      three = 3
  four <- Just 4
  Just ( one + two + three + four )
