data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f1 f2) (Pair a b) = Pair (f1 a) (f2 b)


data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure s = Two mempty s
  (<*>) (Two a1 f) (Two a2 b2) = Two (mappend a1 a2) (f b2)


data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure v = Three mempty mempty v
  (<*>) (Three a1 b1 f) (Three a2 b2 v) = Three (mappend a1 a2) (mappend b1 b2) (f v)


data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a1 f1 f2) (Three' a2 b1 b2) = Three' (mappend a1 a2) (f1 b1) (f2 b2)
