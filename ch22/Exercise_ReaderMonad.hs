{-# LANGUAGE InstanceSigs #-}

newtype Reader r a =
  Reader { runReader :: r -> a }

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

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
       ->  (a -> Reader r b)
       -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb $ ra r) $ r
  -- (Reader ra) >>= aRb = 
  --   Reader $ \r -> aRb (ra r)