module Reps20200421 () where

just10 :: Maybe Int
just10 = Just 10

just10v2 :: Maybe Int
just10v2 = do Just 10

listDo :: [Int]
listDo = do [12,12]

appDo :: Applicative m => m Int
appDo = do pure 10

addOne :: Maybe Int -> Maybe Int
addOne mInt = do
  int <- mInt
  Just (int +1)

add1and2ToList :: [Int] -> [Int]
add1and2ToList list = do
  int <- list
  [int+1,int+2]

add1Monad :: Monad m => m Int -> m Int
add1Monad mIntA = do
  intA <- mIntA
  pure (intA +1)

sumAndProd :: [Int] -> [Int] -> [Int]
sumAndProd listA listB = do
  intA <- listA
  intB <- listB
  [intA+intB,intA*intB]

addTwoMonad :: Monad m => m Int -> m Int -> m Int
addTwoMonad mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  pure (intA + intB)

sumAndProdPair :: Monad m => m Int -> m Int -> m (Int,Int)
sumAndProdPair mIntA mIntB = do
  intA <- mIntA
  intB <- mIntB
  let
    sumInts  = intA +intB
    prodInts = intA *intB
  pure (sumInts,prodInts)

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
putStrLnAlias str=
  putStrLn str

askShow :: String -> IO String
askShow str = do
  _ <- putStrLn str
  getLine

askShowV2 :: String -> IO String
askShowV2 str = do
  putStrLn str
  getLine

-- once explicitly pattern matching on the result of printing the line

secretFunc :: String -> String -> IO (Maybe String)
secretFunc secrect prompt = do
  putStrLn "Enter secrect"
  line <- getLine
  if line == secrect
     then do
       putStrLn prompt
       lineTwo <- getLine
       pure (Just lineTwo)
    else do
      putStrLn "NOPE"
      pure Nothing
{-

join       :: Monad m => m (m a) -> m a
Bind (>>=) :: m a -> (a -> m b) -> m b
"I'd specifically like you to write bind with the arguments flipped,
  so that it mirrors Functor and Applicative"

Write join for List
Write join for Maybe
Write Bind for List
Write Bind for Maybe

Write join for list useing Bind
Write join for Maybe useing Bind
Write Bind for list useing join
Write Bind for Maybe useing join

##### part D
Create newtype for Name, Age, and BirthMonth.
Create Person record with Name Age BirthMonth

1. validate BirthMonth
  :: String -> Maybe BirthMonth {non-empty}
2. validate Number
  :: String -> Maybe Int {use readMaybe}
3. validate Positive
  :: Int -> Maybe Int
4. validate Age
  :: String -> Maybe Age {useing val Number and val Positive}

validate Length
  :: Age -> String -> Maybe String
    if age over 18 then Val else Nothing

validate Letter
  :: BirthMonth -> String -> Maybe String
    BirthMonth and NameString first letter match

Flip coin to see what method to do first Bind or Do.
Once complete one method, copy and paste function
and replace with other method.
Use bind operator

validate Name
  :: BirthMonth -> Age -> String -> Maybe Name
    user valLength and valLetter

validate Person
  :: String -> String -> String -> Maybe Person
    {do useing valBirthMonth valAge valName}

-}
