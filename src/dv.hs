import Text.Read (readMaybe)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma func =
  case ma of
    Nothing -> Nothing
    Just a -> func a

joinMaybe :: Maybe(Maybe a) -> Maybe a
joinMaybe mMa =
  case mMa of
    Nothing -> Nothing
    Just ma -> ma

newtype Name = Name String
newtype Age = Age Int
newtype BirthMonth = BirthMonth String

data Person = Person
  { perName :: Name
  , perAge :: Age
  , perBirthMonth :: BirthMonth
  }

valBirthMonth :: String -> Maybe BirthMonth
valBirthMonth bmString =
  case bmString of
    "" -> Nothing
    anyBm -> Just $ BirthMonth anyBm

valInt :: String -> Maybe Int
valInt string =
  readMaybe string

valPositive :: Int -> Maybe Int
valPositive int =
  if int<(0::Int)
     then Nothing
     else Just int

valAge :: String -> Maybe Age
valAge stringAge =
  Age <$> joinMaybe
            (valPositive <$> valInt stringAge)

valLength :: Age -> String -> Maybe String
valLength (Age ageInt) string =
  if ageInt<(18::Int)
     then Just string
     else Nothing

valLetter :: BirthMonth -> String -> Maybe String
valLetter (BirthMonth bmString) nameString =
  case bmString of
    [] -> Nothing
    (bx:_) ->
      case nameString of
        [] -> Nothing
        (x:_) ->
          if (bx==x)
             then Just nameString
             else Nothing

-- >>=

valName :: BirthMonth -> Age -> String -> Maybe Name
valName bm age nameString =
  valLetter bm nameString >>= \validLetter ->
  valLength age validLetter >>= \validLength ->
  Just $ Name validLength

valName2 :: BirthMonth -> Age -> String -> Maybe Name
valName2 bm age nameString = do
  validLetter <- valLetter bm nameString
  validLength <- valLength age validLetter
  Just $ Name validLength

valPerson :: String -> String -> String -> Maybe Person
valPerson nameString ageString bmString =
  valBirthMonth bmString >>= \validBm ->
  valAge ageString >>= \validAge ->
  valName validBm validAge nameString >>= \validName ->
  Just $ Person validName validAge validBm

valPerson2 :: String -> String -> String -> Maybe Person
valPerson2 nameString ageString bmString = do
  validBm <- valBirthMonth bmString
  validAge <- valAge ageString
  validName <- valName validBm validAge nameString 
  Just $ Person validName validAge validBm
