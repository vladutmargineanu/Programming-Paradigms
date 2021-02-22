squares lst   = [ x * x | x <- lst ]

qSort []      = []
qSort (h : t) =   qSort [ x | x <- t, x <= h ]
              ++  [h]
              ++  qSort [ x | x <- t, x > h ]

interval      = [ 0 .. 10 ]
evenInterval  = [ 0, 2 .. 10 ]
naturals      = [ 0 .. ]