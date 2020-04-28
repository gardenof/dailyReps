module Year2020.Month04.Day28 () where

class HasMagnitude a where
  magnitude :: a -> Integer

instance HasMagnitude Integer where
  magnitude = id

instance HasMagnitude Int where
  magnitude = toInteger

instance HasMagnitude () where
  magnitude = const 0

instance HasMagnitude (a,b) where
  magnitude = const 2

instance HasMagnitude (a,b,c) where
  magnitude = const 3

instance HasMagnitude (Maybe a) where
  magnitude =
    \maybeVal -> maybe 0 (const 1) maybeVal

instance HasMagnitude [a] where
  magnitude =
    \listVal -> magnitude (length listVal)

data Ponzi
  = Victim String
  | Fraudster String [Ponzi]

ponziMagnitude :: Ponzi -> Integer
ponziMagnitude ponzi =
  case ponzi of
    Victim _ -> 1
    Fraudster _ victims ->
      sum (1:map ponziMagnitude victims)

instance HasMagnitude Ponzi where
  magnitude = ponziMagnitude

nullMagnitude :: HasMagnitude a => a -> Bool
nullMagnitude a =
  magnitude a == 0

compareMagnitude :: HasMagnitude a => a -> a -> Ordering
compareMagnitude aOne aTwo =
  compare (magnitude aOne) (magnitude aTwo)

{- did not add sort
sortNull :: HasMagnitude a => [a] -> [a]
sortNull list =
  case list of
    [] -> []
    (x:xs) ->
      if nullMagnitude x
        then
          sortNull xs
        else
          List.sort ((magnitude x):sortNull xs)
-}

{-
##### typeclass
- Define a typeclass, HasMagnitude,
    for types that have can be sized by an Integer
- Provide an instance of HasMagnitude for Integer
- Provide an instance of HasMagnitude for Int
- Provide an instance of HasMagnitude for ()
- Provide an instance of HasMagnitude for 2-tuples
- Provide an instance of HasMagnitude for 3-tuples
- Provide an instance of HasMagnitude for Maybe
- Provide an instance of HasMagnitude for list
  * Use the HasMagnitude instance for Int in your definition
- Define a tree-like datatype describing a Ppononzi scheme
- Provide a function calculate the magnitude of a Ponzi scheme
- Provide an instance of HasMagnitude for your Ponzi scheme type
- Define a polymorphic function that tells whether a type with HasMagnitude
  is null-sized (i.e. has magnitude zero)
- Define a polymorphic function that compares two values by the magnitude

- Define a polymorphic function that filters the null-sized items out of
  a list and sorts the remaining items by magnitude

RUN BUILD FOR ERRORS
-}
