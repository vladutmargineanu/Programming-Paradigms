import Parser
import Data.Char

{-
    Tipul unor expresii aritmetice simple, care pot conține variabile și numere.
-}
data Expr
    = Number Int
    | Var String
    | Add Expr Expr
    deriving Show

{-
    Parser-ul unei variabile.

    >>> var "xy+12"
    [(Var "xy", "+12")]
-}
var :: Parser Char Expr
var = maxList (spot isLetter) `transform` Var

{-
    Parser-ul unui număr întreg.

    Funcția `read` din clasa `Read` permite conversia unui `String`
    la un tip dorit, în cazul de față, `Int`.

    >>> number "12+xy"
    [(Number 12, "+xy")]
-}
number :: Parser Char Expr
number = maxList (spot isDigit) `transform` (Number . read)

{-
    Parser-ul unui atom: variabilă, număr sau expresie parantezată.
-}
atom :: Parser Char Expr
atom =     var
     `alt` number
     `alt` ((token '(' >*> expr >*> token ')') `transform` \(_, (e, _)) -> e)

{-
    Parser-ul general al unei expresii: atom sau adunare de expresii.
-}
expr :: Parser Char Expr
expr =     atom
     `alt` ((atom >*> token '+' >*> atom) `transform` \(x, (_, y)) -> Add x y)

{-
    e1 = Add (Number 12) (Add (Var "ab") (Number 345))
-}
e1 :: Expr
e1 = result expr "12+(ab+345)"