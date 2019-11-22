newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant a
  (Constant f g) <*> (Constant a b) = Constant a (g b)
  -- (<*>) (Constant a f) (Constant a b) = Constant a