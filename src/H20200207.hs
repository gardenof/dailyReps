module H20200207 where

import qualified  Data.Text as T

data Cat = Cat
  { catName :: CatName
  , catAge  :: CatAge
  }

newtype CatAge = CatAge Int
newtype CatName = CatName T.Text

creatACat :: CatName -> CatAge -> Cat
creatACat name age =
  Cat { catName = name
      , catAge  = age
      }

editCatName :: Cat -> CatName -> Cat
editCatName cat name =
  cat { catName = name}

fmapList :: (a -> b) -> [a] -> [b]
fmapList funcationA2B listA =
  case listA of
    []     -> []
    (x:xs) ->
      funcationA2B x : fmapList funcationA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a  -> Just $ functionA2B a

foldlfoldl :: (b -> a -> b) -> b -> [a] -> b
foldlfoldl funcitonBA2B b listA =
  case listA of
    []     -> b
    (x:xs) ->
      foldlfoldl funcitonBA2B (funcitonBA2B b x) xs

foldlfoldlT :: (b -> a -> b) -> b -> [a] -> b
foldlfoldlT funcitonBA2B b listA =
  case listA of
    [] -> b
    (x:xs) ->
      foldlfoldlT funcitonBA2B (funcitonBA2B b x) xs

{-
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
* applMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b

* applyList :: [a -> b] -> [a] -> [b]

* pureMaybe :: a -> Maybe a

* pureList :: a -> [a]

* mebers :: Maybe [String]

* lengthOfEachMeber :: Maybe [String] -> Maybe [Int]
  * Should work like
  * Just ["Bob", "Carol"] -> Just [3,5]

* `~~~~` :: [Maybe String]

* `~~~~` :: [Maybe String] -> [Maybe Int]
-}
