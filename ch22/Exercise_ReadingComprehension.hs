{-# LANGUAGE InstanceSigs #-}

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

newtype Reader r a =
  Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (\r -> f $ ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ (\r -> (rab r) (ra r))