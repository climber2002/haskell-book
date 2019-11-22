f x=
  lookup x [ (3, "hello")
             , (4, "julie")
             , (5, "kbai")]
g y=
  lookup y [ (7, "sup?")
             , (8, "chris")
             , (9, "aloha")]
h z=
  lookup z [(2, 3), (5, 6), (7, 8)]
m x=
  lookup x [(4, 10), (8, 13), (1, 9001)]

validateLength :: Int -> String -> Maybe String
validateLength maxLength s =
  if (length s) > maxLength
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)
mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

data Cow = Cow {
  name :: String,
  age :: Int,
  weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight = Cow <$> noEmpty name <*> noNegative age <*> noNegative weight
