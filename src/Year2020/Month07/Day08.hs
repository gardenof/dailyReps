module Year2020.Month07.Day08 () where

useDollor :: (a -> b) -> a -> b
useDollor f a = f $ a

makeDollar :: (a -> b) -> a ->b
makeDollar f a = f a

functionalDollar :: (a -> b) -> (a -> b)
functionalDollar f = f

trivialUseDollar :: Int
trivialUseDollar = length $ "hello"

useParentheses :: Int
useParentheses = length ("hello"++"world")

useDolorTwo :: Int
useDolorTwo = length $ "hello"++"world"

compose :: (b->c) -> (a->b) -> a -> c
compose f1 f2 a = (f1 . f2) a

createCompose :: (b->c) -> (a->b) -> a -> c
createCompose f1 f2 a = f1(f2 a)

functionalCompose :: (b->c) -> (a->b) -> (a->c)
functionalCompose f1 f2 = f1 . f2

useDolorTwice :: Int
useDolorTwice = length $ words $ "hello"++"world"

useDotAndParentheses :: Int
useDotAndParentheses = (length . words) ("hello"++"world")

useDotAndDollar :: Int
useDotAndDollar = length . words $ "hello"++"world"

onlyDollar :: Int
onlyDollar = length $ words $ concat ["hello","world"]

oneDot :: Int
oneDot = length . words $ concat $ ["hello"++"world"]

twoDots :: Int
twoDots = length . words . concat $ ["hello"++"world"]

allDots :: Int
allDots = (length . words . concat) ["hello"++"world"]

haiku :: String
haiku = "Haskell a day \n\
        \ will keep the \n\
        \ fumbles away"

wordsInLines :: [Int]
wordsInLines = map (length . words) . lines $ haiku

wordsInLines2 :: [Int]
wordsInLines2 = map (length . words) $ lines haiku
