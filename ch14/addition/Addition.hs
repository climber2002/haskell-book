module Addition where

import Test.Hspec
import Test.QuickCheck

-- sayHello :: IO()
-- sayHello = putStrLn "hello!"

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0 where
  go n d count 
    | n < d = (count, n)
    | otherwise = go (n - d) d (count + 1)

summation :: (Eq a, Num a) => a -> a -> a
summation a b = go a b 0 where
  go n d r
    | n == 0 = r
    | otherwise = go (n - 1)  d (d + r)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
  
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing),
             (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO()
runQc = quickCheck prop_additionGreater

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "15 dividedBy 3 is 5" $ do
      divideBy 15 3 `shouldBe` (5, 0)
    it "22 dividedBy 5 is 4 remainder 2" $ do
      divideBy 22 5 `shouldBe` (4, 2)
    it "3 summation 5 is 15" $ do
      summation 3 5 `shouldBe` 15
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)