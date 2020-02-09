## Reps

### Create a ...
* New file with a date naming convention HYearMonthDay
    * Ex: For Jan 7th, 2020 it would be H20200107
* Model
* data record
* Two different new type
* write a function that will create an instance of your record that you just created.
* write a function that will edit part of your record.

### Write the function out
* function that does what fmap does for a list
* function that does what fmap does for a Maybe
* function that does what foldl

### Data types
##### Enumerated (enum) type
Enum is a data type consisting of a set of named values.
* Create a data that has three different Color constructors
* Create a function that goes from your Color to a String of your color.
  * if you give Blue it gives you a Spring that says "Blue"

##### Basic Algebraic Data Type
An algebraic data type (ADT) has one or more data constructors,
and each data constructor can have zero or more arguments.
* Create data Vegetable = Celery | Carrot Color
* Create a function that takes a Vegetable and gives a Color
	* vegetableColor :: Vegetable -> Color
* Create a function that takes a Vegetable and give a String of the color
	* vegetableToString :: Vegetable -> String

##### Polymorphic Algebraic data type
  * Create a data called Keep that has two constructors =Shoebox a | Safe a
  * Create function takeOut :: Keep a -> a
	* Put a Red Carrot into a ShoeBox(Keep) and write out what its type would be
	* Create a ShoeBox Carrot and write out what its type would be

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

* mebers :: Maybe [String]

* applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b

* applyList :: [a -> b] -> [a] -> [b]

* lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
  * Should work like
  * Just ["Bob", "Carol"] -> Just [3,5]

* `~~~~` :: [Maybe String]

* `~~~~` :: [Maybe String] -> [Maybe Int]

