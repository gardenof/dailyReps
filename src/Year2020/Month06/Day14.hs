module Year2020.Month06.Day14 () where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord  as Ord

{-
   Reflexivity : x==y = True
   Symmetry : x==y = y==x
   Transitivity : If x==y And y==z Then X==Z
   Negation : x/=y = Not(x==y)
-}

data Pie
  = Apple
  | Cream
  | Pumpkin

instance Eq Pie where
  left == right =
    case (left,right) of
      (Apple,Apple) -> True
      (Cream,Cream) -> True
      (Pumpkin,Pumpkin) -> True
      _ -> False

data Gym = Gym
  { gymName  :: String
  , gymLevel :: Int
  }

instance Eq Gym where
  left == right =
    gymName left == gymName right
    && gymLevel left == gymLevel right

data Drink
  = Water
  | Soda
  | Juice
  deriving Eq

data Dog = Dog
  { dogName :: String
  , dogAge :: Int
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

homeTownToString :: Hometown -> String
homeTownToString (Hometown homeTown) = homeTown

newtype Age = Age Int

data Person = Person
  { personName :: Name
  , personHomeTown :: Hometown
  , personAge :: Age
  }

personOne :: Person
personOne = Person(Name"Timmy")(Hometown"Moon")(Age 32)

introducing :: Person -> String
introducing person =
  "Hello "
  ++ nameToString (personName person)
  ++ " from "
  ++ homeTownToString (personHomeTown person)

instance Eq Hometown where
  left == right =
    homeTownToString left == homeTownToString right

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
  | Bronze
  | Silver
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
ranks = [Bronze,Bronze,Silver,Gold]

ascendingRanks :: [Rank]
ascendingRanks = List.sort ranks

descindingRanks :: [Rank]
descindingRanks = List.sortOn Ord.Down ranks

data Player = Player
  { playerName :: String
  , playerRank :: Rank
  }

players :: [Player]
players =
  [ Player "e" Gold
  , Player "a" Bronze
  , Player "s" Silver
  ]

alphabetically :: [Player]
alphabetically =
  List.sortOn playerName players

rankedPlayers :: [Player]
rankedPlayers =
  List.sortBy (Ord.comparing playerRank) players

comparePlayers1 :: Player -> Player -> Ordering
comparePlayers1 p1 p2 =
  case Ord.comparing playerName p1 p2 of
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

dollar :: (a->b) -> a -> b
dollar f a = f $ a

noDollar :: (a->b) -> a -> b
noDollar f a = f a

functionToFunction :: (a->b) -> (a->b)
functionToFunction f = f

trivialDollar :: Int
trivialDollar = length $ "something" ++"something two"

parenthesesLength :: Int
parenthesesLength = length ("one"++"two")

dollarLength :: Int
dollarLength = length $ "one"++ "two"

compose :: (a->b) -> (b->c) -> a -> c
compose f1 f2 a = (f2 . f1) a

composeTwo :: (a->b) -> (b->c) -> a -> c
composeTwo f1 f2 a = f2 (f1 a)

functionalCompose :: (a->b) -> (b->c) -> a -> c
functionalCompose f1 f2 = f2 . f1

dollarTwice :: Int
dollarTwice = length $ words $ ("one"++"two")

useDotAndParentheses :: Int
useDotAndParentheses = (length . words) ("one"++"two")

useDotAndDollar :: Int
useDotAndDollar =
  length . words $ "one"++"two"

dollarsX3 :: Int
dollarsX3 = length $ words $ concat $ ["one"++"two"]

dollarsX2 :: Int
dollarsX2 = length . words $ concat $ ["one"++"two"]

dollarsX1 :: Int
dollarsX1 = length . words . concat $ ["one"++"two"]

dollarsX0 :: Int
dollarsX0 = (length . words . concat) ["one"++"two"]

haiku :: String
haiku =
  "Haskell a data \n\
  \keeps the fumble away"

wordsInHaiku :: [Int]
wordsInHaiku =
  map (length . words) . lines $ haiku

wordsInHaikuTwo :: [Int]
wordsInHaikuTwo =
  map (length . words) $ lines haiku

{-
   The Dot and Dollar Operators
   (.) is function composition. (.) was chosen for this operator because it
   vaguely resembles the small centered o used in math to denote function
   composition.
   ($) is function application. It may seem useless to have this because we
   can always apply functions in Haskell just by putting arguments after them.
   ($) is useful though because it has a low operator precendence that allows
   it to be use to avoid excessive parentheses.
   Using (.) and ($) together can get confusing sometimes. These reps will
   help etch the meaning of (.) and ($) into your muscles so that knowing
   when to use each will become a reflex.
   - Implement a `dollar` function that takes a function and an argument
     and uses ($) to apply the function to that argument.
   - Implement a `myDollar` function that takes a function and and argument
     and applies the function to that argument without using dollar. Yes,
     these two functions really do the same thing -- dollar isn't magical.
   - Implement a `functionalDollar` "function" that takes a function a returns
     that function. Make sure to denote this way of thinking in the type signature
     with parentheses . Also note that this function is still identical to the
     other too.
   - Use the dollar operator in a trivial fashion to find the length of a string
   - Use parentheses to find the length of a string that is made by appending two strings
   - Use dollar to find the length of a string made by appending two strings
   - Implement a `compose` function that uses (.) to combine two functions. Your
     function should take all the arguments involved in the type signature
     explicitly.
   - Implement a `myCompose` function that is the same as `compose`, but does
     not use (.). Instead just apply the functions use normal Haskell syntax.
   - Implement a `functionalCompose` function that use (.), but leaves off the
     third argument. That is to say, the only arguments your function should
     explicitly take are the two functions being combined. Use parentheses in
     the type signature to reflect this way of thinking about composition.
   - Use ($) twice to find the number of words in a string that is made by
     appending two strings
   - Use (.) and parenteses to do the same thing
   - Use (.) and ($) together to do it again, using no parentheses
   - Use ($) three times to find the number of words in a string constructed
     by `concat`ing a list of strings.
   - Do the same thing using one (.) and two ($)s
   - Do the same thing using two (.)s and one ($)
   - Do the same thing using three (.)s and paretheses - no ($)s allowed!
   - Compose a haiku in a Haskell String
   - Find number of words on each line of the haiku by using `map`, `words` and `lines`.
   - Do it again, finding a different combination of (.) and ($)s that also works
-}
