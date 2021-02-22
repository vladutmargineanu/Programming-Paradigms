module Functor where

import Types

{-
Clasa "Functor" este predefinita. Ea generalizeaza operatia "map".

class Functor c where
    fmap :: (a -> b) -> c a -> c b
    
Pentru c = [] (constructorul de tip "lista"), fmap = map.
-}

instance Functor Pair where
    fmap f (P x y) = P (f x) (f y)
    
instance Functor NestedList where
    fmap f (Atom x) = Atom $ f x
    fmap f (List l) = List $ map (fmap f) l
    
incN1 = fmap (+ 1) n1
incN2 = fmap (fmap (+ 1)) n2