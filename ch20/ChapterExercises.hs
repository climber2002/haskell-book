data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f initial (Constant b) = f b initial

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f initial (Two a b) = f b initial

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f initial (Three a b c) = f c initial

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldr f initial (Three' a b1 b2) = f b2 (f b1 initial)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldr f initial (Four' a b1 b2 b3) = f b3 $ f b2 $ f b1 initial