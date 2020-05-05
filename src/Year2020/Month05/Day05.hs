module Year2020.Month05.Day05 () where

data Car =
  Car { year :: Int
      , name :: String
      }

createCar :: Car
createCar =
  Car {year=1234,name = "bowbow"}

carConstructor :: Int -> String -> Car
carConstructor = Car

buildCar :: Int -> String -> Car
buildCar int str = Car int str

changeName :: String -> Car -> Car
changeName newName car =
  car {name = newName}

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
    Green -> "Green"
    Blue -> "Blue"
    Red -> "Red"

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

takeKeep :: Keep a -> a
takeKeep keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox =
  Shoebox $ Carrot Blue

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
