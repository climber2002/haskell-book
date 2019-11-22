import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' t = getSum $ foldMap Sum t

product' :: (Foldable t, Num a) => t a -> a
product' t = getProduct $ foldMap Product t

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a t = getAny $ foldMap (\a' -> Any $ a' == a ) t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr maybeMin Nothing

maybeMin :: (Ord a) => a -> Maybe a -> Maybe a
maybeMin x (Just y) = Just $ min x y
maybeMin x _ = Just x

length' :: (Foldable t) => t a -> Int
length' t = getSum $ foldMap (\x -> Sum 1) t

toList' :: (Foldable t) => t a -> [a]
toList' t = foldMap (\a -> [a]) t

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> (f x) <> y) mempty