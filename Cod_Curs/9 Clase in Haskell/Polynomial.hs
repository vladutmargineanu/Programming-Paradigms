{-
    Tipul polinoamelor cu coeficienți de tipul `a`.

    Polinomul (a0 + a1 X + a2 X^2 + ...) este reprezentat prin fluxul
    coeficienților săi, [a0, a1, a2, ...], infinit.

    Motivul pentru care nu utilizăm direct tipul `[a]`, și recurgem
    la împachetarea acestuia în tipul `Polynomial a` vizează instanțierea
    diverselor clase. Astfel, același tip de la bază, precum `[a]`,
    poate contribui la instanțe diferite ale aceleiași clase, din spatele
    unor împachetări diferite. De exemplu, nu am fi putut reinstanția clasa
    `Show` direct cu tipul `[a]` (vezi mai jos), deoarece acesta este deja
    o instanță standard.

    Anumite funcții ale claselor `Num` și `Fractional` sunt lăsate
    neimplementate, fapt ce va genera warning-uri.

    Adaptare după:
    http://www.seas.upenn.edu/~cis194/fall14/spring13/hw/06-laziness.pdf
-}
data Polynomial a = Poly [a]

instance Show a => Show (Polynomial a) where
    -- Luăm în calcul primii 20 de coeficienți
    show (Poly bs) = show $ take 20 bs
    
instance Num a => Num (Polynomial a) where 
    -- O constantă este interpretată drept un polinom în care toți coeficienții
    -- cu rang mai mare sau egal cu 1 sunt 0.
    fromInteger = Poly . (: repeat 0) . fromInteger
    
    -- Negarea unui polinom presupune negarea tuturor coeficienților.
    negate (Poly bs) = Poly $ map negate bs
    
    -- Adunarea polinoamelor
    Poly bs + Poly cs = Poly $ zipWith (+) bs cs
    
    -- Înmulțirea polinoamelor presupune exploatarea următoarei observații
    -- și a evaluării leneșe:
    -- B = b0 + x B', C = c0 + x C'
    -- BC = (b0 + x B') C = b0 C + x B' C = b0 (c0 + x C') + x B' C
    --    = b0 c0 + x (b0 C' + B' C)
    Poly (b0 : bs') * Poly cs@(c0 : cs') = Poly $ (b0 * c0) : rs
      where
        Poly rs = Poly (map (* b0) cs') + Poly bs' * Poly cs
        
instance Fractional a => Fractional (Polynomial a) where
    -- Împărțirea polinoamelor presupune exploatarea următoarei observații
    -- și a evaluării leneșe:
    -- Dacă B / C = R, atunci R = b0/c0 + x 1/c0 (B' - R C')
    Poly (b0 : bs') / Poly (c0 : cs') = result
      where
        Poly rs = Poly bs' - result * Poly cs'
        result  = Poly $ (b0 / c0) : map (/ c0) rs
        
instance Functor Polynomial where
    -- Aplicarea unei funcții asupra fiecărui coeficient.
    fmap f (Poly bs) = Poly $ map f bs
    
-- Polinomul X, reprezentat ca [0, 1, 0, 0, ...]
x :: Num a => Polynomial a
x = Poly $ 0 : 1 : repeat 0

-- Polinomul ai cărui coeficienți sunt numerele Fibonacci.
fibo :: Fractional a => Polynomial a
fibo = x / (1 - x - x^2)