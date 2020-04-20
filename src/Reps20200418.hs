module Reps20200418 () where

maybeInt :: Maybe Int
maybeInt =
  Just 10

maybeIntDo :: Maybe Int
maybeIntDo = do
  Just 10

listDo :: [Int]
listDo = do
  [12,12]

pureApplicative :: Applicative m => m Int
pureApplicative = do
  pure 10

addOneToMaybe :: Maybe Int -> Maybe Int
addOneToMaybe maInt = do
  int <- maInt
  Just (int+1)

addOneTwoToList :: [Int] -> [Int]
addOneTwoToList listOfInt = do
  int <- listOfInt
  [int +1,int+2]


addOneMonad :: Monad m => m Int -> m Int
addOneMonad mint = do
  int <- mint
  pure (int + 1)

listPlusList :: [Int] -> [Int] -> [Int]
listPlusList listOne listTwo = do
  one <- listOne
  two <- listTwo
  [one+two]

addtwoIntsMonad :: Monad m => m Int -> m Int -> m Int
addtwoIntsMonad intOne intTwo = do
  one <- intOne
  two <- intTwo
  pure ( one + two)

{-
##### do
- Construct a Maybe Integer value as usual
- Demonstrate a trivial use a do with Maybe defining the same value as above
- Demonstrate a similar trivial use of do with List
- Demonstrate a trivial polymorphic use of do using `pure` (i.e. one that works for all Applicatives)
- Use do syntax to define a function that adds 1 to a `Maybe Integer`
- Use do syntax to define a function that adds takes a
  list of Integers and adds 1 and 2 to each one, creating a single list of results
- Use do syntax to define a polymorphic function that adds 1 to an Integer within any Monad
- Use do syntax to define a function that takes two lists and
  returns a list that contains the sums and products of all the
  combinations of items from the two list.
- Use do syntax to define a function that adds two Integer values within any Monad

- Use do syntax to define a function that returns the pair of the product and
  sum of two Integer within a Monad
  * use let inside the two to assign variables names for the sum and product

- Define an alias for `getLine` with the correct type signature
+ Define an IO operation that finds the length of a line of input from the user
  - once using do syntax
  - once using fmap
- Use fmap to do the same thing
- Define an alias for `putStrLn` with the correct type signature
+ Define a function that takes a string and builds an IO that show it as a prompt on the screen, then gets a line of input in three ways:
  - once explicitly ignoring the result of printing the prompt (i.e. using `<-`)
  - once implicitly ignoring the result of printing (i.e. without using `<-`)
  - once explicitly pattern matching on the result of printing the line
- Define a function that takes a secret string and a prompt string. It should first
  ask the user to ender the secret. If the secret they enter matches the secret given to
  the function, then use do syntax to show the prompt, get their answer and return it.
  If the secret doesn't match, use do syntax to print a message telling them so
  and then return Nothing from the operation.
-}
