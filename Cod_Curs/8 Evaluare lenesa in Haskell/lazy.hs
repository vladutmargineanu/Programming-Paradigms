{-
    Adapted from
    "Haskell: The Craft of Functional Programming", 2nd ed, S. Thompson.
-}

import Data.List ((\\))

{-
    The evaluation strategy allows for writing functions very ellegantly,
    in terms of lists and list operations. However, this is done
    in a very efficient manner, since the lists are never completely built.
-}

sumSquares n = sum (map (^2) [1 .. n])

{-
    = sum (map (^2) 1 : [2 .. n])
    = sum (1^2 : (map (^2) [2 .. n]))
    = 1^2 + sum (map (^2) [2 .. n])
    = 1 + sum (map (^2) [2 .. n])
    ...
    = 1 + (4 + sum (map (^2) [3 .. n]))
    ...
    = 1 + (4 + (9 + ... + n^2))
-}

{-
    Similarly, the minimum of a list may be defined as the first element
    of the list, after it has been sorted using insertion sort. As shown below,
    the computation involves a number of steps which is linear in the list
    length.
-}

ins x []        = [x]
ins x (h : t)
    | x <= h    = x : h : t
    | otherwise = h : (ins x t)

isort []        = []
isort (h : t)   = ins h (isort t)

minList l = head (isort l)

{-
    minList [3, 2, 1]
    = head (isort [3, 2, 1])
    = head (isort (3 : [2, 1]))
    = head (ins 3 (isort [2, 1]))
    = head (ins 3 (isort (2 : [1])))
    = head (ins 3 (ins 2 (isort [1])))
    = head (ins 3 (ins 2 (isort (1 : []))))
    = head (ins 3 (ins 2 (ins 1 (isort []))))
    = head (ins 3 (ins 2 (ins 1 [])))
    = head (ins 3 (ins 2 (1 : [])))
    = head (ins 3 (1 : ins 2 []))
    = head (1 : (ins 3 (ins 2 [])))
    = 1
-}

{-
    The accessibility between two nodes in a directed graph can be assessed
    by solving another problem first: finding all the routes between
    the two nodes. Lazy evaluation guarantees that the computation
    shall backtrack only as to determine the first route,
    according to "accessible".

    The list "explored" is used to prevent going into infinite loops.
-}

theGraph = [(1, 2), (1, 4), (2, 1), (2, 3),
            (3, 5), (3, 6), (5, 6), (6, 1)]

neighbors node = map snd . filter ((== node) . fst)

routes source dest graph explored
    | source == dest = [[source]]
    | otherwise      = [ source : path
                       | neighbor <- neighbors source graph \\ explored
                       , path <- routes neighbor dest graph (source : explored)
                       ]

accessible source dest graph =
    (routes source dest graph []) /= []

-- neighbors node graph = map snd (filter (\pair -> fst pair == node) graph)

{-
    A dynamic programming problem using lazy evaluation.

    `payable s` returns `True` if the sum of money `s` can be paid using
    coins of 5 and 7.

    `v` stores on the `i`th position whether the sum `i` can be paid.
    Initially, only the first element (index 0) is computed (`True`).
    Lazy evaluation allows for the construction of `v` in a top-down manner, 
    starting with sum we are actually interested in (`s`). If the computation
    for some index requires the value at a lower index (which is usually
    the case), the latter is generated recursively on the fly and stored
    for future use.

    Note that `v` and `f` are mutually recursive. The construction of `v`
    requires `f`, and the computation of `f` requires `v`. However, once
    a certain element of `v` has been computed, it is used as is, and no
    further application of `f` is required.

    Adapted from:
    https://wiki.haskell.org/Dynamic_programming_example
-}
payable :: Int -> Bool
payable s = v !! s
  where
    v = True : map f [1 .. s]
    f i = i - 5 >= 0 && v !! (i - 5) ||
          i - 7 >= 0 && v !! (i - 7)