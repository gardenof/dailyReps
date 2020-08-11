module Year2020.Month08.Day11 () where

just10 :: Maybe Int
just10 = Just 10

trivialMaybeDo :: Maybe Int
trivialMaybeDo = do Just 10

trivialListDo :: [Int]
trivialListDo = do [10,22]

trivialPolymorphicDo :: Applicative m => m Int
trivialPolymorphicDo = do pure 10

maybeAddOne :: Maybe Int -> Maybe Int
maybeAddOne maybeInt = do
  int <- maybeInt
  Just (int + 1)

addOneAndTwoToAll :: [Int] -> [Int]
addOneAndTwoToAll ints = do
  int <- ints
  [int + 1,int + 2]

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
monadicSumAndProduct ma mb = do
  a <- ma
  b <- mb
  let
    sumAB = a+b
    productAB = a*b
  pure (sumAB,productAB)

getUserInput :: IO String
getUserInput = getLine

lengthOfLine1 :: IO Int
lengthOfLine1 = do
  line <- getLine
  pure ( length line)

lengthOfLine2 :: IO Int
lengthOfLine2 =
  fmap length getLine

printLineToScreen :: String -> IO ()
printLineToScreen = putStrLn

askUserInput1 :: String -> IO String
askUserInput1 prompt = do
  putStrLn prompt
  getLine

askUserInput2 :: String -> IO String
askUserInput2 prompt = do
  _ <- putStrLn prompt
  getLine

askUserInput3 :: String -> IO String
askUserInput3 prompt = do
  () <- putStrLn prompt
  getLine

askUserCodeInput :: String -> String -> IO (Maybe String)
askUserCodeInput realCode prompt = do
  putStrLn "whats the code"
  userEnteredCode <- getLine

  if userEnteredCode == realCode
    then do
      putStrLn prompt
      userEntered <- getLine
      pure $ Just userEntered
    else do
      putStrLn "Wrong Code"
      pure Nothing
