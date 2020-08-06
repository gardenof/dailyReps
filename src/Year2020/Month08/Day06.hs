module Year2020.Month08.Day06 () where

just10 :: Maybe Int
just10 = Just 10

trivialMaybeDo :: Maybe Int
trivialMaybeDo = do Just 10

trivialListDo :: [Int]
trivialListDo = do [10,12]

trivialPolymorphicDo :: Applicative m => m Int
trivialPolymorphicDo = do pure 10

maybeAddOne :: Maybe Int -> Maybe Int
maybeAddOne maybeInt = do
  int <- maybeInt
  Just (int+1)

addOneAndTwoToAll :: [Int] -> [Int]
addOneAndTwoToAll ints = do
  int <- ints
  [int + 1, int + 2]

polymorphicAddOne :: Monad m => m Int -> m Int
polymorphicAddOne mInt = do
  int <- mInt
  pure (int+1)

allSumAndProducts :: [Int] -> [Int] -> [Int]
allSumAndProducts as bs = do
  a <- as
  b <- bs
  [a+b,a*b]

monadicSumAndProduct :: Monad m => m Int -> m Int -> m (Int, Int)
monadicSumAndProduct mA mB = do
  a <- mA
  b <- mB

  let
    sumAB     = a+b
    productAB = a*b

  pure (sumAB, productAB)
