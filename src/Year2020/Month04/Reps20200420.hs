module Year2020.Month04.Reps20200420 () where

just10 :: Maybe Int
just10 =
  Just 10

just10Do :: Maybe Int
just10Do = do
  Just 10

listdo :: [Int]
listdo = do
  [12,12]

applicativePure :: Applicative m => m Int
applicativePure = pure 10

add1 :: Maybe Int -> Maybe Int
add1 maInt = do
  int <- maInt
  pure (int+1)

add1And2ToList :: [Int] -> [Int]
add1And2ToList list = do
  int <- list
  [int+1,int+2]

add1Monad :: Monad m => m Int -> m Int
add1Monad  mInt = do
  int <- mInt
  pure (int+1)

sumAndProOfLists :: [Int] -> [Int] -> [Int]
sumAndProOfLists listA listB = do
  intA <- listA
  intB <- listB
  [intA+intB,intA*intB]

addTwo :: Monad m => m Int -> m Int -> m Int
addTwo mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  pure (intA+intB)

pairSumProd :: Monad m => m Int -> m Int -> m (Int, Int)
pairSumProd mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  let
    sumInts = intA + intB
    proInts = intA * intB
  pure (sumInts,proInts)

getlineAlias :: IO String
getlineAlias =
  getLine

getLineLength :: IO Int
getLineLength = do
  line <- getLine
  pure (length line)

getLineLengthV2 :: IO Int
getLineLengthV2 =
  fmap length getLine

putStrLnAlias :: String -> IO ()
putStrLnAlias str =
  putStrLn str

showAndAsk :: String -> IO String
showAndAsk prompt = do
  _ <- putStrLn prompt
  getLine

showAndAskV2 :: String -> IO String
showAndAskV2 prompt = do
  putStrLn prompt
  getLine

-- once explicitly pattern matching on the result of printing the line

secrectCheck :: String -> String -> IO (Maybe String)
secrectCheck secretStr promptStr = do
  putStrLn "enter secret"
  gotSecret <- getLine
  if gotSecret == secretStr
     then do
       putStrLn promptStr
       gotLine <- getLine
       pure (Just gotLine)
     else do
       putStrLn "NOPE!!"
       pure Nothing
