module BTerm where

import LTerm

data BTerm = Leq LTerm LTerm | Conj BTerm BTerm | Neg BTerm
    deriving Show

bsem :: BTerm -> (Vars -> Double) -> Bool
bsem (Leq t1 t2) m =
    let r1 = sem t1 m
        r2 = sem t2 m in r1 <= r2
bsem (Conj b1 b2) m =
    let r1 = bsem b1 m
        r2 = bsem b2 m in r1 && r2
bsem (Neg b) m =
    let v = bsem b m in not v

-- -- b = t <= t'
-- b = Leq t t'
-- -- c = not b
-- c = Neg b