module Year2020.Month07.Day07 () where

useDollor :: (a->b) -> a -> b
useDollor f a = f $ a

makeDollar :: (a->b) -> a ->b
makeDollar f a = f a

functionalDollar :: (a->b) -> (a->b)
functionalDollar f = f

trivialUseDollar :: Int
trivialUseDollar = length $ "hello"

useParentheses :: Int
useParentheses = length ("hello"++"world")

useDolorTwo :: Int
useDolorTwo = length $ "hello"++"World"

compose :: (b->c) -> (a->b) -> a -> c
compose f1 f2 a = (f1 . f2) a

createCompose :: (b->c) -> (a->b) -> a -> c
createCompose f1 f2 a = f1(f2 a)

functionalCompose :: (b->c) -> (a->b) -> (a->c)
functionalCompose f1 f2 = f1 . f2

useDolorTwice :: Int
useDolorTwice = length $ words $ "hello"++"World"

useDotAndParentheses :: Int
useDotAndParentheses = (length . words) ("hello"++"world")

useDotAndDollar :: Int
useDotAndDollar = length . words $ "hello"++"world"

onlyDollar :: Int
onlyDollar = length $ words $ concat ["hello","worl"]

oneDot :: Int
oneDot = length . words $ concat $ ["hello"++"world"]

twoDots :: Int
twoDots = length . words . concat $ ["hello"++"world"]

allDots :: Int
allDots = (length . words . concat) ["hello"++"world"]

haiku :: String
haiku = "Haskell a day\n\
        \ will keep the\n\
        \ fumbles away"

wordsInLines :: [Int]
wordsInLines = map (length . words) . lines $ haiku

wordsInLines2 :: [Int]
wordsInLines2 = map (length . words) $ lines haiku
{-
   The Dot and Dollar Operators
   (.) is function composition. (.) was chosen for this operator because it
   vaguely resembles the small centered o used in math to denote function
   composition.
   ($) is function application. It may seem useless to have this because we
   can always apply functions in Haskell just by putting arguments after them.
   ($) is useful though because it has a low operator precendence that allows
   it to be use to avoid excessive parentheses.
   Using (.) and ($) together can get confusing sometimes. These reps will
   help etch the meaning of (.) and ($) into your muscles so that knowing
   when to use each will become a reflex.
   - Implement a `dollar` function that takes a function and an argument
     and uses ($) to apply the function to that argument.
   - Implement a `myDollar` function that takes a function and and argument
     and applies the function to that argument without using dollar. Yes,
     these two functions really do the same thing -- dollar isn't magical.
   - Implement a `functionalDollar` "function" that takes a function a returns
     that function. Make sure to denote this way of thinking in the type signature
     with parentheses . Also note that this function is still identical to the
     other too.
   - Use the dollar operator in a trivial fashion to find the length of a string
   - Use parentheses to find the length of a string that is made by appending two strings
   - Use dollar to find the length of a string made by appending two strings
   - Implement a `compose` function that uses (.) to combine two functions. Your
     function should take all the arguments involved in the type signature
     explicitly.
   - Implement a `myCompose` function that is the same as `compose`, but does
     not use (.). Instead just apply the functions use normal Haskell syntax.
   - Implement a `functionalCompose` function that use (.), but leaves off the
     third argument. That is to say, the only arguments your function should
     explicitly take are the two functions being combined. Use parentheses in
     the type signature to reflect this way of thinking about composition.
   - Use ($) twice to find the number of words in a string that is made by
     appending two strings
   - Use (.) and parenteses to do the same thing
   - Use (.) and ($) together to do it again, using no parentheses
   - Use ($) three times to find the number of words in a string constructed
     by `concat`ing a list of strings.
   - Do the same thing using one (.) and two ($)s
   - Do the same thing using two (.)s and one ($)
   - Do the same thing using three (.)s and paretheses - no ($)s allowed!
   - Compose a haiku in a Haskell String
   - Find number of words on each line of the haiku by using `map`, `words` and `lines`.
   - Do it again, finding a different combination of (.) and ($)s that also works
-}
