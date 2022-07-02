module ListLogDur where

import Cp
import DurationMonad
import ListDur

data ListLogDur a = LSD [Duration (String, a)] deriving Show

remLSD :: ListLogDur a -> [Duration (String, a)]
remLSD (LSD x) = x

instance Functor ListLogDur where
    fmap f = LSD . (map (fmap (id >< f))) . remLSD

instance Applicative ListLogDur where
    pure = LSD . pure . pure . (\x -> ([], x))
    l1 <*> l2 = LSD $ do
        x <- remLSD l1
        y <- remLSD l2
        g(x,y) where
            g(Duration (d1,(s,f)), Duration (d2,(s',x))) = return (Duration (d1 + d2, (s++s', f x)))

instance Monad ListLogDur where
    return = pure
    l >>= k = let k' = LD . remLSD . k
                  l' = (LD . remLSD) l in
                  LSD $ remLD (l' >>= (auxLSDMonad k')) where
                    auxLSDMonad :: (x -> ListDur (String, y)) -> ((String, x) -> ListDur (String, y))
                    auxLSDMonad k (s, x) = (LD . map (Duration . (id >< ((++ s) >< id)) . remDur) . remLD . k) x
                    remDur (Duration a) = a



