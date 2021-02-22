add1 x y = x + y
add2     = \x y -> x + y
add3     = \x -> \y -> x + y

result   = add1 1 2  -- sau ((add1 1) 2)
inc      = add1 1    -- functie