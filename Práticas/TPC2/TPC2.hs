module TPC2 where

import LTerm
import BTerm

data WhPr = Asg Vars LTerm | Wa Double WhPr | Seq WhPr WhPr | Ife BTerm WhPr WhPr | Wh BTerm WhPr
    deriving Show


-- função de alteração de uma memória: alteração do valor da variável v para r
chMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
chMem v r sigma u = if u == v then r else sigma u

wpsem :: WhPr -> (Vars -> Double) -> (Double, Vars -> Double)
-- semântica de atribuição de valor a uma variável: a memória é alterada (chMem)
-- tempo de execução é 0 
-- output é a nova memória com o valor da variável alterado
wpsem (Asg v t) sigma = (0, chMem v (sem t sigma) sigma)
-- semântica da regra wait:
-- tempo de execução é o tempo de espera 'm' + tempo de execução de p 'n'
-- o output é a memória alterada pelo programa p
wpsem (Wa m p) sigma =
    let (n,sigma') = wpsem p sigma in (m+n,sigma')
-- semântica da regra seq:
-- tempo de execução é o tempo de execução de p 'n' + tempo de execução de q 'n'
-- output é a memória alterada por p, alterada por q
wpsem (Seq p q) sigma =
    let (n,sigma') = wpsem p sigma
        (m,sigma'') = wpsem q sigma' in (n+m, sigma'')
-- semântica da regra if (if1 e if2):
-- tempo de execução é o tempo de execução de p se b for true (if1)
    -- ou o tempo de execução de q se b for false (if2)
-- o output é a memória alterada por p se b for true
    -- ou a memória alterada por q se b for false
wpsem (Ife b p q) sigma =
    if bsem b sigma then wpsem p sigma else wpsem q sigma
-- semântica da regra while (wh1 e wh2)
-- tempo de execução é 
    -- se b for true, o tempo de execução de p mais o tempo de execução do programa while
    -- se b for false, 0
-- o output é a memória alterada sequencialmente por p, se b for verdade
    -- ou a memória intacta se b for falso
wpsem (Wh b p) sigma =
    if bsem b sigma
        then
            let (n,sigma') = wpsem p sigma
                (m,sigma'') = wpsem (Wh b p) sigma' in (n+m,sigma'')
        else (0,sigma)


-- valor de X
m X = 1
-- valor de Y
m Y = 2
-- valor de Z
m Z = 3

-- var x com valor de X
x = Leaf (Left X)
-- var y com valor de Y
y = Leaf (Left Y)
-- var z com valor de Z
z = Leaf (Left Z)

-- programas de teste:
-- se x < y, esperar 3 segundos e x = x+z
    -- senão, esperar 5 segundos e x = x+y
-- resultado esperado, usando a memória m definida em cima:
    -- (3,(X := 4))
test_1 = Ife (Leq x y) (Wa 3 (Asg X (Add x z))) (Wa 5 (Asg X (Add x y)))
-- enquanto x < z, x = x+y e, depois, y = y+z
-- resultado esperado, usando a memória m definida em cima:
    -- (0,(X := 8, Y := 8))
test_2 = Wh (Leq x z) (Seq (Asg X (Add x y)) (Asg Y (Add y z)))