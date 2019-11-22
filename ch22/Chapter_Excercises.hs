module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' a [] = Nothing
lookup' a' ((a, b) : ts) = if a' == a
                        then Just b
                        else lookup' a' ts 

-- zip x and y using 3 as the lookup' key
xs :: Maybe Integer
xs = lookup' 3 (zip x y)

-- zip y and z using 6 as the lookup' key
ys :: Maybe Integer
ys = lookup' 6 (zip y z)

zs :: Maybe Integer
zs = lookup' 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup' n (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z1, z2) where
          z1 = z' n
          z2 = z' n

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
bolt i = (i > 3) && (i < 8)

bolt' :: Integer -> Bool
bolt' = (&&) <$> (>3) <*> (< 8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just a') = a'

main :: IO () 
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ fromMaybe s'

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)
