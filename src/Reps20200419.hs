module Reps20200419 () where

maybeIntDo :: Maybe Int
maybeIntDo = do Just 10

listDo :: [Int]
listDo = do
  [12,13]

pureApplicative :: Applicative m => m Int
pureApplicative = do pure 10

addOneToMaybe :: Maybe Int -> Maybe Int
addOneToMaybe maInt = do
  int <- maInt
  Just (int +1)

addOneTwoToList :: [Int] -> [Int]
addOneTwoToList listOfList = do
  int <- listOfList
  [int+1,int+2]

addOneMonad :: Monad m => m Int -> m Int
addOneMonad mInt = do
  int <- mInt
  pure (int+1)

twolistaddTimes :: [Int] -> [Int] -> [Int]
twolistaddTimes listA listB = do
  intA <- listA
  intB <- listB
  [intA+intB,intA*intB]

addTwoMonad :: Monad m => m Int -> m Int -> m Int
addTwoMonad intAm intBm = do
  inta <- intAm
  intb <- intBm
  pure (inta+intb)

sumPodPair :: Monad m => m Int -> m Int -> m (Int,Int)
sumPodPair mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  let
    sum = intA+intB
    product = intA*intB
  pure(sum,product)

getLineIO :: IO String
getLineIO =
  getLine

getLineLength :: IO Int
getLineLength = do
  line <- getLine
  pure (length line)

getLineLengthV2 :: IO Int
getLineLengthV2 =
  fmap length getLine

printToScreen :: String -> IO ()
printToScreen string= putStrLn string

stringToScreen :: String -> IO String
stringToScreen str = do
  _ <- putStrLn str
  getLine

stringToScreenV2 :: String -> IO String
stringToScreenV2 string = do
  putStrLn string
  getLine

-- once explicitly pattern matching on the result of printing the line

secret :: String -> String -> IO (Maybe String)
secret mainSecr prompt = do
  putStrLn "enter secret"
  askedSecr <- getLine

  if mainSecr == askedSecr
     then do
       putStrLn prompt
       sndAsk <- getLine
       pure (Just sndAsk)
     else do
       putStrLn "Wonge"
       pure Nothing

