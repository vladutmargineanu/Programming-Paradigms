add5 0 y          = y      -- add5 1 2
add5 (x + 1) y    = 1 + add5 x y

listSum []        = 0      -- sumList [1, 2, 3]
listSum (hd : tl) = hd + listSum tl

pairSum (x, y)    = x + y  -- sumPair (1, 2)

wackySum (x, y, z@(hd : _)) =   -- wackySum
    x + y + hd + listSum z      --  (1, 2, [3, 4, 5])