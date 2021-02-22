add6     :: Integer -> Integer -> Integer
add6 x y =  x + y

f        :: (Integer -> Integer) -> Integer
f g      =  (g 3) + 1

idd      :: a -> a  -- functie polimorfica
idd x    =  x       -- a: variabila de tip!