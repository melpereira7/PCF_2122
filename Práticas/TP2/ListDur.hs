module ListDur where

import Cp
import DurationMonad

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
    fmap f = LD . (map (fmap f)) . remLD

instance Applicative ListDur where
    pure = LD . pure . pure
    l1 <*> l2 = LD $ do x <- remLD l1
                        y <- remLD l2
                        g(x, y) where
                        g :: Monad m => (Duration (t -> a), Duration t) -> m (Duration a)
                        g(Duration (d1, f), Duration (d2, x)) =
                          return $ Duration (d1 + d2, f x)

instance Monad ListDur where
    return = pure
    l >>= k = LD $ do
      x <- remLD l
      g x k where
        g :: Duration t -> (t -> ListDur a) -> [Duration a]
        g (Duration (d, a)) k = 
          map (\(Duration (d', a)) -> (Duration (d + d', a))) (remLD (k a))

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)