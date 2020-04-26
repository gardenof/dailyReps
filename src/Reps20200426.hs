module Reps20200426 () where

just10 :: Maybe Int
just10 = Just 10

just10Do :: Maybe Int
just10Do = do Just 10

listDo :: [Int]
listDo = do [11,11]

appDo :: Applicative m => m Int
appDo = do pure 10

addone :: Maybe Int -> Maybe Int
addone mint = do
  int <- mint
  Just (int+1)

add1and2ToList :: [Int] -> [Int]
add1and2ToList listA = do
  int <- listA
  [int+1,int+2]

add1Monad :: Monad m => m Int -> m Int
add1Monad mInt = do
  int <-mInt
  pure(int+1)

sumAndProd :: [Int] -> [Int] -> [Int]
sumAndProd listA listB = do
  intA <- listA
  intB <- listB
  [intA+intB,intA*intB]

addTwoMonad :: Monad m => m Int -> m Int -> m Int
addTwoMonad mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  pure (intA+intB)

sumAndProdPair :: Monad m => m Int -> m Int -> m(Int,Int)
sumAndProdPair mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  let
    sumInts = intA+intB
    prodInts = intA*intB
  pure(sumInts,prodInts)

getLineAlias :: IO String
getLineAlias = getLine

getLineLength :: IO Int
getLineLength = do
  line <- getLine
  pure(length line)

getLineLengthV2 :: IO Int
getLineLengthV2 =
  fmap length getLine

putStrLnAlias :: String -> IO ()
putStrLnAlias str =
  putStrLn str

askShow :: String -> IO String
askShow str = do
  _ <- putStrLn str
  getLine

askShowV2 :: String -> IO String
askShowV2 str = do
  putStrLn str
  getLine

secretFunc :: String -> String -> IO (Maybe String)
secretFunc secret prompt = do
  putStrLn "enter secret"
  line <- getLine
  if line == secret
     then do
       putStrLn prompt
       lineTwo <- getLine
       pure (Just lineTwo)
     else do
       putStrLn "NOPE"
       pure Nothing
