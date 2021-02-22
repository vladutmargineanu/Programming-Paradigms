module Container where

import Types

class Container t where
    contents :: t a -> [a]

instance Container Pair where  -- nu (Pair a)!
    contents (P x y) = [x, y]

instance Container NestedList where
    contents (Atom x) = [x]
    contents (List l) = concatMap contents l

instance Container [] where
    contents = id