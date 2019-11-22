{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi { runMoi :: s -> (a,s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\s -> 
                          let (a, newS) = g s in (f a, newS))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) = Moi (\s -> 
                              let (a, newS) = g s
                                  (bf, newS2) = f newS in (bf a, newS2))

instance Monad (Moi s) where
  return = pure

  (>>=):: Moi s a 
      -> (a -> Moi s b)
      -> Moi s b
  (Moi f) >>= g = Moi (\s -> 
                        let (a, newS) = f s
                            (b, newS2) = runMoi (g a) newS
                        in (b, newS2))