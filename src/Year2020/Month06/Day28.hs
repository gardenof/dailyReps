module Year2020.Month06.Day28 () where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord  as Ord

newtype Name = Name String

nameToString :: Name -> String
nameToString (Name name) = name

newtype Hometown = Hometown String

hometownToString :: Hometown -> String
hometownToString (Hometown homeTown) = homeTown

newtype Age = Age Int

data Person = Person
  { personName :: Name
  , personHomeTown :: Hometown
  , personAge :: Age
  }

personOne :: Person
personOne = Person(Name"ee")(Hometown"FL")(Age 9001)

introducing :: Person -> String
introducing person =
  "Hello "
  ++ (nameToString $ personName person)
  ++ " from "
  ++ (hometownToString $ personHomeTown person)

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
      (_,Gold) -> LT
      (Silver,_) -> GT
      (_,Silver) -> LT

ranks :: [Rank]
ranks = [Silver,Silver,Bronze,Gold]

ascendingRank :: [Rank]
ascendingRank =
  List.sort ranks

descindingRanks :: [Rank]
descindingRanks =
  List.sortOn Ord.Down ranks

data Player = Player
  { playerName :: String
  , playerRank :: Rank
  }

players :: [Player]
players =
  [ Player "a" Gold
  , Player "s" Bronze
  , Player "s" Silver
  ]

alphabeticalPlayers :: [Player]
alphabeticalPlayers =
  List.sortOn playerName players

rankedPlayers :: [Player]
rankedPlayers =
  List.sortBy (Ord.comparing playerRank) players

comparePlayers1 :: Player -> Player -> Ordering
comparePlayers1 p1 p2 =
  case Ord.comparing playerRank p1 p2 of
    GT -> GT
    LT -> LT
    EQ -> Ord.comparing playerName p1 p2

comparePlayers2 :: Player -> Player -> Ordering
comparePlayers2 p1 p2 =
  Ord.comparing playerRank p1 p2 <> Ord.comparing playerName p1 p2

comparePlayers3 :: Player -> Player -> Ordering
comparePlayers3 =
  Ord.comparing playerRank <> Ord.comparing playerName

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
