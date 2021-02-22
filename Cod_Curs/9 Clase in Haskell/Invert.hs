module Invert where

import Types

class Invert a where
    invert :: a -> a
    invert = id

instance Invert (Pair a) where
    invert (P x y) = P y x

instance Invert a => Invert (NestedList a) where
    invert (Atom x) = Atom (invert x)
    invert (List x) = List $ reverse $ map invert x

instance Invert a => Invert [a] where
    invert lst = reverse $ map invert lst

instance Invert Double
instance Invert Int
instance Invert Integer
instance Invert Float
instance Invert Char
instance Invert Bool