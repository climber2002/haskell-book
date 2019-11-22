data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg


data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Right' b) = Right' b
  fmap f (Left' a) = Left' $ f a

instance Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  (<*>) _ (Right' b) = Right' b
  (<*>) (Right' f) _ = Right' f
  (<*>) (Left' f) (Left' a) = Left' $ f a

instance Monad (PhhhbbtttEither b) where
  (>>=) (Left' a) f = f a
  (>>=) (Right' b) _ = Right' b


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a rest) f = append (f a) (rest >>= f)
  
j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f m = fmap f m

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 f ma mb = f <$> ma <*> mb
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b


-- l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2' f ma mb =  ma >>= f

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
-- meh (a : as) f = (>>=) (f a) (\x -> fmap (x:) (meh as f))
meh (x:xs) f = do
  x' <- f x
  xs' <- meh xs f
  return $ x':xs'

flipType :: (Monad m) => [m a] -> m [a]
flipType [] = pure []
flipType ms = meh ms id