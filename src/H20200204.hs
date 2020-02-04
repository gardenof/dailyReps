module H20200204
  ()where

import qualified  Data.Text as T

data Adidas = Adidas
  { adidasName :: AdidasName
  , adidasAge  :: AdidasAge
  }

newtype AdidasAge = AdidasAge Int
newtype AdidasName = AdidasName T.Text

createAdidas :: AdidasAge -> AdidasName -> Adidas
createAdidas age name =
  Adidas { adidasName = templetAdidasName, adidasAge = templetAdidasAge}

templetAdidasAge :: AdidasAge
templetAdidasAge =
  AdidasAge 21

templetAdidasName :: AdidasName
templetAdidasName =
  AdidasName $ T.pack"Jimmy"

editAdidasAge :: Adidas -> AdidasAge -> Adidas
editAdidasAge record age =
  record { adidasAge = age}

editAdidasName :: Adidas -> AdidasName -> Adidas
editAdidasName record name =
  record { adidasName = name }

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just x  -> Just $ functionA2B x

foldlfoldl :: (b -> a -> b) -> b -> [a] -> b
foldlfoldl funcitonBA2B b listA =
  case listA of
    [] -> b
    (x:xs) -> foldlfoldl funcitonBA2B (funcitonBA2B b x) listA


{-

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
-}
