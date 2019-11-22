{-# LANGUAGE InstanceSigs #-}

newtype Identity a = 
  Identity { runIdentity :: a } deriving Show

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
          Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

newtype One f a =
  One (f a) deriving (Eq, Show)

instance Functor f =>
  Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) =>
  Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- Exercise
instance (Applicative f, Applicative g) => 
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (<*>) (Compose f) (Compose fga) =
    Compose $ ((<*>) <$> f <*> fga)
  -- (<*>) (Compose fn) (Compose x) = 
  --   Compose $ (convert fn) <*> x where 
  --     convert :: f (g (a -> b)) -> f (g a -> g b)
  --     convert h = (pure (<*>)) <*> h

