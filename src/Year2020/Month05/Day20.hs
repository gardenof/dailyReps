module Year2020.Month05.Day20 () where

data Car = Car
  { carName :: String
  , carYear :: Int
  }

createCarRec :: String -> Int -> Car
createCarRec nameStr yearInt =
  Car {carName = nameStr, carYear = yearInt}

carConstructor :: String -> Int -> Car
carConstructor = Car

carConstructorReg :: String -> Int -> Car
carConstructorReg s i = Car s i

updateName :: String -> Car -> Car
updateName name car =
  car {carName = name}

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

insideKeep :: Keep a -> a
insideKeep keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox =
  Shoebox $ Carrot Red

carrotInShoebox :: Keep (Color -> Vegetable)
carrotInShoebox = Shoebox Carrot

{-
Enumerated (enum) type
  Enum is a data type consisting of a set of named ws values.
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
  Reflexivity x==y = True
  Symmetry x==y = y==x
  Transitivity x==y = y==z then x==z
  Negation x/=y = not(x==y)
-}

data Pie
  =Apple
  |Pumpkin
  |Rhubarb

instance Eq Pie where
  left == right =
    case (left,right) of
      (Apple,Apple) -> True
      (Pumpkin,Pumpkin) -> True
      (Rhubarb,Rhubarb) -> True
      _ -> False

data RecipeIngredient =
  RecipeIngredient
    {recName :: String
    ,recFeed :: Int
    }

instance Eq RecipeIngredient where
  left == right =
    recName left == recName right
    && recFeed left == recFeed right

data Drink
  = Water
  | Soda
  | Juice
  deriving Eq

data Meal = Meal
  { meanName :: String
  , mealFeeds :: Int
  }deriving Eq


{-
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
-}

newtype Name = Name String

nameToString :: Name -> String
nameToString (Name nameStr) =
  nameStr

newtype Hometown = Hometown String

hometownToString :: Hometown -> String
hometownToString (Hometown homeTownStr) =
  homeTownStr

newtype Age = Age Int

data Person = Person
  { personName :: Name
  , personHomeTown :: Hometown
  , personAge :: Age
  }

personOne :: Person
personOne =
  Person
    {personName = Name "EE"
    , personHomeTown = Hometown "home"
    , personAge = Age 22
    }

introduction :: Person -> String
introduction person =
  "Hello"
  ++ (nameToString (personName person))
  ++ (hometownToString (personHomeTown person))

{--
  Newtypes
    | A "newtype" is a type defined with tha `newtype` keyword. It is a
    | trivial wrapper around an existing type that creates a brand new type
    | out of it that the compiler will treat as different from the original
    | type. This is useful for many things, including distinguishing between
    | values that would otherwise be the same simple type. The compiler
    | removes newtype wrappers at compile time so that there is littre to
    | no runtime cost associated with having many newtypes.
- Define a type using newtype to represent a person's Name
- Implement a function nameToString to convert a Name to String
- Define a type using newtype to represent a person's Hometown
- Implement a function hometownToString to convert a Hometown to String
- Define a type using newtype to represent a person's Age
- Define a Person record using these three types.
- Construct a value of type Person.
- Implement a function to create a String introducing a Person by stating their
  name and where they are from. Be polite and omit their age from the
  introduction.
| Newtypes can also be used create a wrapper for the purpose of providing
| a new typeclass instance for a type that already has one. We will use this
| technique in these reps occasionally to allow us to re-write the typeclass
| implementations in common libraries as a way to understand them better.
- define an Eq instance for Hometown that is equivalent to the one for String
- define an Eq instance for Name that compares names in a case-insensitive manner.
--}
