front (x : y : zs)  =   x + y
front [x]           =   x

notNil []           =   False
notNil (_ : _)      =   True

f m n
	| notNil xs     =   front xs
	| otherwise     =   n
  where
    xs              =   [m .. n]