data List a = 
    Nil 
  | Cons a (List a) 
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
  pure a = ZipList' $ (Cons a Nil)
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' xs) = ZipList' (append (f <$> xs) (fs <*> xs))
