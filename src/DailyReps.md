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

##### part C
validate BirthMonth :: String -> Maybe BirthMonth {non-empty}

validate NameVtwo :: BirthMonth -> Age -> String -> Maybe Name

validate PersonVtwo   :: String -> String -> Maybe Person {do useing 1 & 4}

Name Validators
Must start with same letter as BirthMonth
Name Must be 2 words if age is over 18

Do them all with in both bind and join versions?
New person validation
create PersonTwo with Name Age BirthMonth
validate PersonTwo :: String -> String -> String -> Maybe PersonTwo
  Creat one with bind and but don't use <$> or <*>

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

##### Let and In
- Write out abstract let form
- Calculate 8, using let to bind 4 to a name
- Calculate 9, multiplying a let variable, three, by itself
- Calculate 10 using two let expression, one for 5 and one for 2
- Calculate 10 again using one let expression for both 5 and 2
- Calculate 8 again, using a lambda to simulate let
- Calculate 10 again, using a lambda to simulate let
- Calculate 5 with a single let, start with two, multiplying by itself to get 4,
  defining 1 and then adding that to four. (4 bindings in total, including 5 itself)
- Calculate 5 again as above, but this time order the let bindings in
  decreasing order of magnitude (starting with 5, ending with 1)
- Build a list of 1000 ones by using a recursive let to build an infinite list
  and then using `take 1000`
- Build an infinite list of alternating ones and zeros using a mutually
  recursive let
- Use a let in a do expression. Add Just 1, 2, 3, and Just 4, binding each
  one to a name using either let or `<-`

##### do
- Construct a Maybe Integer value as usual
- Demonstrate a trivial use a do with Maybe defining the same value as above
- Demonstrate a similar trivial use of do with List
- Demonstrate a trivial polymorphic use of do using `pure` (i.e. one that works for all Applicatives)
- Use do syntax to define a function that adds 1 to a `Maybe Integer`
- Use do syntax to define a function that adds takes a list of Integers and adds 1 and 2 to each one, creating a single list of results
- Use do syntax to define a polymorphic function that adds 1 to an Integer within any Monad
- Use do syntax to define a function that takes two lists and returns a list that contains the sums and products of all the combinations of items from the two list.
- Use do syntax to define a function that adds two Integer values within any Monad
- Use do syntax to define a function that returns the pair of the product and sum of two Integer within a Monad
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

RUN BUILD FOR ERRORS
