module Year2020.Month06.Day17 () where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord  as Ord

newtype Name = Name String

nameToString :: Name -> String
nameToString (Name name) = name

newtype HomeTown = HomeTown String

homeTownToString :: HomeTown -> String
homeTownToString (HomeTown homeTown) = homeTown

newtype Age = Age Int

data Person = Person
  { personName :: Name
  , personHomeTown :: HomeTown
  , personAge :: Age
  }

personOne :: Person
personOne = Person(Name"a")(HomeTown"Moon")(Age 22)

introducing :: Person -> String
introducing person =
  "Hello"
  ++ (nameToString $ personName person)
  ++ " From "
  ++ (homeTownToString $ personHomeTown person)

instance Eq HomeTown where
  left == right =
    homeTownToString left == homeTownToString right

instance Eq Name where
  left == right =
    let
      nameLower = map Char.toLower . nameToString
    in
      nameLower left == nameToString right

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
ranks = [Bronze, Silver, Gold, Gold]

ascendingRanks :: [Rank]
ascendingRanks =
  List.sort ranks

descindingRank :: [Rank]
descindingRank =
  List.sortOn Ord.Down ranks

data Player = Player
  { playerName :: String
  , playerRank :: Rank
  }

players :: [Player]
players =
  [ Player "q" Gold
  , Player "w" Silver
  , Player "a" Silver
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

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    x:_ -> Just x
    _   -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail list =
  case list of
    _:xs -> Just xs
    _    -> Nothing

safeLast :: [a] -> Maybe a
safeLast list =
  case list of
    [x]  -> Just x
    _:xs -> safeLast xs
    _    -> Nothing

names :: [String]
names = ["James", "johon", "tim", "Smith"]

lengthsOfNames :: [Int]
lengthsOfNames = map length names

totalLengthOfNames :: Int
totalLengthOfNames = sum lengthsOfNames

roster :: String
roster = List.intercalate ", " names

isLongName :: String -> Bool
isLongName string = length string > 7

areAllNamesLong1 :: Bool
areAllNamesLong1 =
  and (map isLongName names)

areAllNamesLong2 :: Bool
areAllNamesLong2 =
  all isLongName names

anyNameIsLong1 :: Bool
anyNameIsLong1 =
  or (map isLongName names)

anyNameIsLong2 :: Bool
anyNameIsLong2 =
  any isLongName names

justLongNames :: [String]
justLongNames =
  filter isLongName names

-- longNames, shortNames ::
-- isDarrylLong ::

aLongName :: Maybe String
aLongName =
  List.find isLongName names

shortenName :: String -> String
shortenName =
  take 7

remainingName :: String -> String
remainingName =
  drop 7

shortenNameWithRemainder :: String -> (String,String)
shortenNameWithRemainder = splitAt 7

-- isEnglishVowel ::
-- dropLeadingVowels ::
-- takeLeadingVowels ::
-- splitOffLeadingVowels ::
-- splitOffLeadingConsonants ::

{-
These reps will help build you knowledge about how lists work and some of the
functions available in the List API. List has far too many functions to cover
in a single set of reps, so we'll have to revisit List again in the future to
see some more advanced functions.
Note: Many of the list functions we use here are re-exported but the standard
Haskell Prelude, but some are not. You'll need to import Data.List to do some
of these reps. Also, some of these functions actually work with more types than
just List, but we're not going to worry about that for now.
- Implement a function, `safeHead`, that is like `head`, but produces `Maybe a`
  when the list is empty rather than raising an nasty runtime error.
- Implement a function, `safeTail`, that is like `tail`, but produces `Maybe [a]`
  when the list is empty instead of raising an error.
- Implement a function, `safeLast`, that is like `last`, but produces `Maybe a`
  when the list is empty instead of raising an error.
Note: The moral of these three functions is that you should really never
use `head`, `tail` and `last` because they can produce a runtime error and
there is almost always a better way.
- Construct a list of names of various lengths
- Use the list functions `map` and `list` to build a list of all the lengths of
  the names.
- Use `sum` to find the total lengths of all the names together.
- Use `intercalate` to constructor a printable roster of all the names in
  the list separated by a comma and space.
- Write a function to determine if a name is long (longer than 7 characters)
- Use `and` and `map` to determine if all the names in your list are long
- Use `all` to do the same thing, without needing to use `map`
- Use `or` and `map` to determine if any of the names in you list are long
- Use `any` to do the same thing, without needing `map`
- Use `filter` to build a list of just the long names
- Use `partition` to separate the list of names into long and short names in
  a single pass.
Note: The partition example below demonstrates that you can bind multiple top
level values at the same time by pattern matching on a tuple. Although this is
rarely used in real life, it's good to know. Otherwise it's might be extremely
confusing should you happen upon an example in the wild.
- Pick one of the example names and use `elem` to see if is in the list of
  long names.
- Use `find` to pick out the first long name from the list.
- Write a function to shorten a name to make it short using `take`
- Write a function to find what would remainder would be lost by shortening using `drop`.
- Write a function to shorten a name and return the remainder simultaneous using `splitAt`.
- Use `elem` write a function `isEnglishVowel` to check whether a character is
  an English vowel.
- Write a function to remove the leading vowels from a name using `dropWhile`
- Write a function to extract the leading vowels from a name using `takeWhile`
- Write a function to split off the leading vowels and return the remainder together using `span`
- Write a function to split off the leading consonants and return the remainder together using `break`
-}
