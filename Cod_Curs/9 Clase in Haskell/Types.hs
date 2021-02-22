module Types where

data Pair a = P a a

data NestedList a
    = Atom a
    | List [NestedList a]

instance Show a => Show (Pair a) where
    show (P x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Show a => Show (NestedList a) where
    show (Atom x) = show x
    show (List l) = show l

n1 = List [Atom 1, List [Atom 2, Atom 3], Atom 4, Atom 5]
n2 = List [ Atom $ P 1 2
          , Atom $ P 3 4
          , List [ Atom $ P 5 6
                 , List [ Atom $ P 7 8 ]
                 ]
          , Atom $ P 9 10
          ]

nAppend ax@(Atom _) ay@(Atom _)  = List [ax, ay]
nAppend ax@(Atom _) (List y)     = List $ ax : y
nAppend (List x)     ay@(Atom _) = List $ x ++ [ay]
nAppend (List x)     (List y)    = List $ x ++ y