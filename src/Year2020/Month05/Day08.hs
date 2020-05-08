module Year2020.Month05.Day08 () where

data Car = Car
  { carName :: String
  , carYear :: Int
  }

makeCarRecord :: String -> Int -> Car
makeCarRecord nameStr yearint =
  Car {carName = nameStr, carYear = yearint}

carConstructor :: String -> Int -> Car
carConstructor = Car

carConstructorReg :: String -> Int -> Car
carConstructorReg nameStr yearInt =
  Car nameStr yearInt

updateName :: String -> Car -> Car
updateName newNameStr car =
  car {carName = newNameStr}

{-
Basics of Haskell Records
- Create a record with two fields of different types
- write a function that will create an instance of your record using record syntax
- write down the type of the data constructor for your record
- write a function that will create an instance of your record using regular data constructor syntax
- write a function that will update one of the fields of an existing record with a new value
-}


data Color
  = Green
  | Blue
  | Red

colorToString :: Color -> String
colorToString col =
  case col of
    Green -> "green"
    Blue -> "blue"
    Red -> "red"

data Vegetable
  = Celery
  | Carrot Color

vegToColor :: Vegetable -> Color
vegToColor veg =
  case veg of
    Celery -> Green
    Carrot col -> col

vegToString :: Vegetable -> String
vegToString veg =
  colorToString $ vegToColor veg

data Keep a
  = Shoebox a
  | Safe a

insideKepp :: Keep a -> a
insideKepp keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox =
  Shoebox $ Carrot Red

carrotInShoebox :: Keep (Color -> Vegetable)
carrotInShoebox =
  Shoebox Carrot

{-
Enumerated (enum) type
  Enum is a data type consisting of a set of named values.
  - Create a data that has three different Color constructors
  - Create a function that goes from your Color to a String of your color.
Basic Algebraic Data Type
  An algebraic data type (ADT) has one or more data constructors,
  and each data constructor can have zero or more arguments.
  - Create type to represent Vegetables. Celery has data arguments. Carrots
    need a color.
  - Create a function that takes a Vegetable and gives its Color.
  - Create a function that takes a Vegetable and give a String of the color.
Polymorphic Algebraic data type
  - Create a type called Keep that can story anything. Things can be kept in
    either a Shoebox or a Safe.
  - Create a function to take items out of the Keep.
  - Put a Red Carrot into a Shoebox(Keep) and write out what its type would be.
  - Create a Shoebox Carrot and write out what its type would be.
-}

{-
Reflexivity : x == x True

Symmetry : x == y = y == x

Transitivity :
  IF x == y and y == z then x == z

Negation :
  x /= y = not (x == y)
-}

data Pie
  = Apple
  | Pumpkin
  | Rhubarb

instance Eq Pie where
  left == right =
    case (left, right) of
      (Apple, Apple) -> True
      (Pumpkin,Pumpkin) -> True
      (Rhubarb,Rhubarb) -> True
      _ -> False

data RecipeIngredient =
  RecipeIngredient
    { recName :: String
    , recFeeds :: Int
    }

instance Eq RecipeIngredient where
  left == right =
    recName left == recName right
    && recFeeds left == recFeeds right

data Drink
  = Water
  | Soda
  | Juice
  deriving Eq

data Meal = Meal
  { meanName :: String
  , mealFeeds :: Int
  }deriving Eq

{--
Eq
  | Eq is a typeclass for types that can be compared for equality.
- Write out the Eq "Laws"
  * Note that the Haskell Report that defines the official Haskell language
    does not give an official laws for Eq. Despite this, there are customary
    laws given in the docs for Eq that represent the general expected practice
    for implementing Eq. The laws given in the answer here are those, with
    one omission (Substitutivity), for simplicities sake.
  - Reflexivity
  - Symmetry
  - Transitivity
  - Negation
- Define an enum type and provide an Eq instance for it
- Define a record type and provide an Eq instance for it
- Define another enum type and derive Eq for it
- Define another record type and derive Eq for it
--}
