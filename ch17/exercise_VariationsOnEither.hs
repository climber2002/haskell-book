-- Validation
data Validation err a = Failure err | Success a deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left e) = Failure e
eitherToValid (Right a) = Success a

data Errors = 
    DividedByZero
  | StackOverflow
  | MoogleChewedWires
  deriving (Eq, Show)

-- success = Success (+1) <*> Success 1

instance Functor (Validation e) where
  fmap f (Failure err) = Failure err
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Failure e1) (Failure e2) = Failure (mappend e1 e2)
  (<*>) (Failure e1) (Success a) = Failure e1
  (<*>) (Success f) (Failure e) = Failure e
  (<*>) (Success f) (Success a) = Success (f a)
