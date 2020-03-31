module Reps20200327 () where

fmapList :: (a -> b) -> [a] -> [b]
fmapList func listA =
  case listA of
    [] -> []
    (x:xs) ->
      func x : fmapList func xs

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe func ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ func a

createfoldl :: (b -> a -> b) -> b -> [a] -> b
createfoldl func b listA =
  case listA of
    [] -> b
    (x:xs) ->
      createfoldl func (func b x) xs

data Color
  = Blue
  | Green
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

vegtoString :: Vegetable -> String
vegtoString veg =
  colorToString $ vegToColor veg

data Keep a
  = ShoeBox a
  | Safe a

takeOut :: Keep a -> a
takeOut keep =
  case keep of
    ShoeBox insideShoebox -> insideShoebox
    Safe insideSafe -> insideSafe

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = ShoeBox $ Carrot Red

shoeBoxCarrot :: Keep (Color -> Vegetable)
shoeBoxCarrot = ShoeBox Carrot

data MakeMaybe a
  = NothingMaybe
  | JustMaybe a

mapMakeMaybe :: (a -> b) -> MakeMaybe a -> MakeMaybe b
mapMakeMaybe func ma =
  case ma of
    NothingMaybe -> NothingMaybe
    JustMaybe a -> JustMaybe $ func a

instance Functor MakeMaybe where
  fmap = mapMakeMaybe

data MakeList a
  = NothingList
  | MakeList a (MakeList a)

mapMakeList :: (a -> b) -> MakeList a -> MakeList b
mapMakeList func lista =
  case lista of
    NothingList -> NothingList
    MakeList x xs ->
      MakeList (func x) (mapMakeList func xs)

instance Functor MakeList where
  fmap = mapMakeList

pureMaybe :: a -> Maybe a
pureMaybe a = Just a

pureList :: a -> [a]
pureList a = [a]

members :: Maybe [String]
members = Just ["asd","asd","sa"]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe mFun ma =
  case mFun of
    Nothing -> Nothing
    Just func ->
      case ma of
        Nothing -> Nothing
        Just a ->
          Just $ func a

applyList :: [a -> b] -> [a] -> [b]
applyList listOfFunc listA =
  case listOfFunc of
    [] -> []
    (func:restOfFuncs) ->
      case listA of
        [] -> []
        (x:_) ->
          func x : applyList restOfFuncs listA


{-
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
  :: String -> Maybe Age {useing 2 & 3}

validate Length
  :: Age -> String -> Maybe String
    if age over 18 then Val else Nothing

validate Letter
  :: BirthMonth -> String -> Maybe String
    BirthMonth and NameString first letter match

validate Name
  :: BirthMonth -> Age -> String -> Maybe Name
    user valLength and valLetter

validate Person
  :: String -> String -> String -> Maybe Person
    {do useing valBirthMonth valAge valName}

RUN BUILD FOR ERRORS
-}
