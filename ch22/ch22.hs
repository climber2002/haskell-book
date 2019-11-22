import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName,
  dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") 
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = 
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog 
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
          
