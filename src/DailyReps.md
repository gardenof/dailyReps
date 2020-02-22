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

* Rewrite the fucntions you wrote with infix notation to use operators
* Create code behind the operator (.) Function composition.
  * Create code behind the operator (.) Function composition.
  * write (.) four diffrent time, with 0,1,2,3 lamds and put "("
    in the types sig to express what you wrote

* Rep that build IO Int that give the length of a string that
  is typed into getLine. Use the functor instance of IO

* build a data type Record with two string filds . use applicative sintake
  that will buld an IO REcord^ by asking for each fild to be entered as a line.

* ^^ do both in the repl.
