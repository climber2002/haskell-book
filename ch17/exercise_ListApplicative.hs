data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) a Nil = a
  (<>) Nil a = a
  (<>) (Cons a1 tail1) c2 = Cons a1 (tail1 <> c2)  

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a tail) = Cons (f a) (fmap f tail)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f tailF) (Cons a tail) = Cons (f a) (fmap f tail) <> ((<*>) tailF (Cons a tail))