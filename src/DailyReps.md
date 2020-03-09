## Reps

### Create a ...
* Model in a new file
* -data record-
* -Two different new type-
* -write a function that will create an instance of your record that you just created.-
* -write a function that will edit part of your record.-

### Write the function out
* -function that does what fmap does for a list-
* -function that does what fmap does for a Maybe-
* -function that does what foldl-

### Data types
##### Enumerated (enum) type
-Enum is a data type consisting of a set of named values.-
* -Create a data that has three different Color constructors-
* -Create a function that goes from your Color to a String of- your color.
  * -if you give Blue it gives you a Spring that says "Blue"-

##### Basic Algebraic Data Type
-An algebraic data type (ADT) has one or more data constructors,-
-and each data constructor can have zero or more arguments.-
* -Create data Vegetable = Celery | Carrot Color-
* -Create a function that takes a Vegetable and gives a Color-
	* -vegetableColor :: Vegetable -> Color-
* -Create a function that takes a Vegetable and give a String of the color-
	* -vegetableToString :: Vegetable -> String-

##### Polymorphic Algebraic data type
  * -Create a data called Keep that has two constructors =Shoebox a | Safe a-
  * -Create function takeOut :: Keep a -> a-
	* -Put a Red Carrot into a ShoeBox(Keep) and write out what its type would be-
	* -Create a ShoeBox Carrot and write out what its type would be-

##### Create your own function
* Make Maybe
	* implement map for your Maybe
  * create Functor instance for your Maybe

* Make List
	* implement map for your List
  * create Functor instance for your List

##### Apply / Pure
* pureMaybe :: a -> Maybe a

* pureList :: a -> [a]

* members :: Maybe [String]

* applyMaybe ::

* applyList ::

* applyZipList ::

* lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
  * Should work like
  * Just ["Bob", "Carol"] -> Just [3,5]
* write with your functions and then fmap BOTH

* `~~~~` :: [Maybe String]

* lengthOfEachMeberTwo :: [Maybe String] -> [Maybe Int]
* write with your functions and then fmap BOTH

##### Pre applicative

* Create a data that is a String
* Create a :: Maybe String
* Use fmap to create an instance of that data type using the maybe value.
* Create a data type that needs a String and Int
* Craete a :: Maybe Int
* Use fmap and apply to create an instance of that data type (String Int) using the the two Maybe values

use ApplyList
 newRepFunction :: List EdenStringIntInt
 newRepFunction =

Once you better understand using applyList
start using infix notation for AppleList and applyMaybe ex= `functionName`

createInstance :: Maybe EdenString
Write something with fmap "create insatnce" that only useing fmap.
-- Now right that with out fmap. Use PureMaybe and applyMaybe maybe.
-- Now that you wrote one for Myabe write one for List

Create applicative instance for MakeMaybe and MakeList FOR pure and (<*>)

instance Applicative MakeList where
  pure =
  (<*>) =

* Rewrite the functions you wrote with infix notation to use operators
* Create code behind the operator (.) Function composition.
  * Create code behind the operator (.) Function composition.
  * write (.) four different times, with 0,1,2,3 Lambdas
  and change "(" in the type to respent the changes
  * write wiht \a b c ->

* Build IO Int that gives the length of a string that is typed into getLine.
* Use the functor instance of IO

* build a data Record with two string fields. Use applicative syntax that will build an IO Record^ by asking for each field to be entered as a line.

* ^^ do both in the REPL.


Create a new type around List
Implement Applicative for it
Provide a pure and apply instance
Make pure be correct for applyZip

Understand "->" in types
figure out the Type for Fmap, Pure, (<*>)
figure out the method for Fmap, Pure, (<*>)

##### Monad
##### Part A
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

##### Part B
(=<<) :: (a -> mb) -> ma -> mb "fliped bind"
(>>=) :: ma -> (a -> mb) -> mb

1. validate Name     :: String -> Maybe Name {Can't be blank "" }
2. validate Number   :: String -> Maybe Int {use read Maybe}
3. validate Positive :: Int -> Maybe Int
4. validate Age      :: String -> Maybe Age {useing 2 & 3}
5. validate Person   :: String -> String -> Maybe Person {do useing 1 & 4}

