module Year2020.Month04.Day27 () where

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
  magnitude = maybe 0 (const 1)

instance HasMagnitude [a] where
  magnitude = magnitude . length

data PonziScheme
  = Victim String
  | Fraudster String [PonziScheme]

ponziSchemeMagnitude :: PonziScheme -> Integer
ponziSchemeMagnitude scheme =
  case scheme of
    Victim _ -> 1
    Fraudster _ victims ->
      case victims of
        [] ->
          0
        (_:xs) ->
          sum $ [1] <> (map ponziSchemeMagnitude xs)

instance HasMagnitude PonziScheme where
  magnitude = ponziSchemeMagnitude

nullSized :: HasMagnitude a => a -> Bool
nullSized a =
  magnitude a == 0

compareMagnitude :: HasMagnitude a => a -> a -> Ordering
compareMagnitude a b =
  compare (magnitude a) (magnitude b)


sortNonNull ::  HasMagnitude a => [a] -> [a]
sortNonNull list =
  case list of
    [] -> []
    (x:xs) ->
      case nullSized x of
        True -> sortNonNull xs
        False -> x:sortNonNull xs
-- NOT SHORTED

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
- Define a tree-like datatype describing a Ponzi scheme
- Provide a function calculate the magnitude of a Ponzi scheme
- Provide an instance of HasMagnitude for your Ponzi scheme type
- Define a polymorphic function that tells whether a type with HasMagnitude
  is null-sized (i.e. has magnitude zero)
- Define a polymorphic function that compares two values by the magnitude

- Define a polymorphic function that filters the null-sized items out of
  a list and sorts the remaining items by magnitude

RUN BUILD FOR ERRORS
-}
