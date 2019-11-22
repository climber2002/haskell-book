newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- foldr :: (a -> b -> b) -> b -> t a -> b
instance Foldable Identity where
  foldr f b (Identity a) = f a b

instance Traversable Identity where
  -- Applicative f => (a -> f b) -> t a -> f (t b)
  traverse func (Identity a) = Identity <$> func a


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = Constant <$> pure x  

-- instance Foldable (Constant a) where
--   foldr f b (Constant a) = b

-- instance Traversable (Constant a) where
--   traverse func (Constant a) = Constant a

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr f b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons h t) = f h `mappend` foldMap f t