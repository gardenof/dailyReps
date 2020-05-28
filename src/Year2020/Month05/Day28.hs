module Year2020.Month05.Day28 () where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord  as Ord

data Color
  = Green
  | Blue
  | Red

colorToString :: Color -> String
colorToString color =
  case color of
    Green -> "green"
    Blue -> "blue"
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

insideKeep :: Keep a -> a
insideKeep keep =
  case keep of
    Shoebox a -> a
    Safe a -> a

redCarrotInShoebox :: Keep Vegetable
redCarrotInShoebox = Shoebox $ Carrot Red

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
Transitivity x==y = y==z Then x==z
Negation x/=y = Not (x==y)
-}

data Pie
  = Apple
  | Rhubarb
  | Pumpkin

instance Eq Pie where
  left == right =
    case (left,right) of
      (Apple,Apple) -> True
      (Rhubarb,Rhubarb) -> True
      (Pumpkin,Pumpkin) -> True
      _ -> False

data Gym = Gym
  { nameGym :: String
  , levelGym :: Int
  }

instance Eq Gym where
  left == right =
    nameGym left == nameGym right
    && levelGym left == levelGym right

data Drink
  = Water
  | Juice
  | Soda
  deriving Eq

data GymLeader = GymLeader
  { leaderName :: String
  , leadersLevel :: Int
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
nameToString (Name name) = name

newtype Hometown = Hometown String

hometownToString :: Hometown -> String
hometownToString (Hometown hometown) = hometown

newtype Age = Age Int

data Person = Person
  { personName :: Name
  , personHomeTown :: Hometown
  , personAge :: Age
  }

personOne :: Person
personOne = Person (Name "EE") (Hometown "house") (Age 18)

introduction :: Person -> String
introduction person =
  "Hello"
  ++ nameToString (personName person)
  ++ " from "
  ++ hometownToString (personHomeTown person)

instance Eq Hometown where
  left == right =
    hometownToString left == hometownToString right

instance Eq Name where
  left == right =
    let
      nameLower = map Char.toLower . nameToString
    in
      nameLower left == nameLower right

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

data Rank
  = Gold
  | Silver
  | Bronze
  deriving Eq

instance Ord Rank where
  compare left right =
    case (left,right) of
      (Gold,Gold) -> EQ
      (Silver,Silver) -> EQ
      (Bronze,Bronze) -> EQ
      (Gold,_) -> GT
      (_,Gold) -> GT
      (Silver,_) -> GT
      (_,Silver) -> GT

ranks :: [Rank]
ranks = [Silver,Gold,Bronze,Silver,Bronze,Gold]

ascendingRanks :: [Rank]
ascendingRanks =
  List.sort ranks

descendingRanks :: [Rank]
descendingRanks =
  List.sortOn Ord.Down ranks

data Player = Player
  { playerName :: String
  , playerRank :: Rank
  } deriving (Eq)

players :: [Player]
players =
  [ Player "E" Gold
  , Player "D" Gold
  , Player "A" Gold
  , Player "M" Silver
  , Player "D" Silver
  , Player "O" Bronze
  ]

alphabeticalPlayers :: [Player]
alphabeticalPlayers =
  List.sortBy (Ord.comparing playerName) players

{-
rankedPlayers :: [Player]
rankedPlayers =
  list.sortOn playerRank players
-}

{--
  Data.Ord / Sorting lists
  - define an enum for player rank and build a manual Ord instance for it
  - sort a list of ranks into ascending order
  - sort a list of ranks into descinding ordering using Ord.Down
  - define a Player record with name and rank
      - Sort a list of players alphabetically using sortBy / comparing
      - Sort a list of players by rank using sortOn
  - Build a compare function for players that compares by name and then rank
      - Hint: use Ord.comparing to compare the fields
      - build one that is fully explicit
      - Then build one that uses the Ordering Semigroup
      - Then build one that uses the Ordering Semigroup and the (->) Semigroup
--}
