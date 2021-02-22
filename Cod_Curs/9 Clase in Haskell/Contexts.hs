module Contexts where

import Invert
import Container

fun1 :: Eq a => a -> a -> a -> a
fun1 x y z = if x == y then x else z

fun2 :: (Container a, Invert (a b), Eq (a b))
     => (a b) -> (a b) -> [b]
fun2 x y = if (invert x) == (invert y)
           then contents x
           else contents y

fun3 :: Invert a => [a] -> [a] -> [a]
fun3 x y = (invert x) ++ (invert y)

fun4 :: Ord a => a -> a -> a -> a
fun4 x y z = if x == y
             then z
             else if x > y
                  then x
                  else y