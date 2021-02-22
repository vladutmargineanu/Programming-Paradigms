import Data.List

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]  -- []
perms xs = [ x : p | x <- xs, p <- perms (xs \\ [x]) ]