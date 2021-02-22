import Data.List hiding (insert)

-- Propriile implementări ale lui foldl și foldr
mfoldl f s [] = s
mfoldl f s (h : t) = mfoldl f (f s h) t

mfoldr f s [] = s
mfoldr f s (h : t) = f h (mfoldr f s t)

{-
    foldr funcționează pe liste infinite dacă și numai dacă funcția de compunere
    a elementului curent și acumulatorului este nestrictă (i.e. își evaluează
    la cerere) în al doilea parametru (acumulatorul, care corespunde aplicației
    recursive).
-}
finite_foldr = mfoldr (\x acc -> if x <= 10
                                 then x + acc
                                 else 0)
                      0