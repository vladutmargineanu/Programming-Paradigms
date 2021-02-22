{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PQueue where

import Data.Maybe
import Data.List (sort)
import TestPP

{-
    Următoarele exerciții vor avea drept scop implementarea unei mici biblioteci
    pentru o coadă de priorități (mai specific, un max priority queue - "primul" element
    are prioritatea cea mai mare).
    Coada de priorități va fi reprezentată folosind o listă sau un arbore binar.
    Biblioteca noastră va defini o reprezentare generală pentru coada de priorități,
    precum și funcții care operează pe aceasta.
-}

 ----------------------------------------------------------------------------------------------------
{- Setup Testare -}

valsInt = [20, 100, 30, 500, 1000, 30023, 513]
valsStr = ["PP", "PA", "PC", "AA", "LFA", "IA", "ML"]
prios = [5, 2, 10, -3, 1, 20]
elemsInt = zip prios valsInt
elemsStr = zip prios valsStr

-- Considerăm că un element din coadă este reprezentat de un tuplu care va conține:
-- * prioritatea
-- * valoarea
type Prio = Int

{-
    1.(1.5p) Analizați clasa PQueue definită mai jos și scrieți implementările
    implicite pentru funcțiile din această clasă:
    * fromList
    * toList

    Clasa PQueue definește interfața pentru toate structurile de coada de priorități
    pe care le vom implementa mai jos.
-}

class (Ord a) => PQueue pq a where

    -- Construiește o coadă de priorități goală
    empty :: pq a

    -- Verifică dacă coada este goală
    isEmpty :: pq a -> Bool

    -- Inserează elem in coada cu priorități
    insert :: (Prio, a) -> pq a -> pq a

    -- Întoarce primul element din coada de priorități
    top :: pq a -> Maybe (Prio, a)

    -- Șterge primul element din coada de priorități
    -- Dacă coada nu are elemente se va returna coada goală
    pop :: pq a -> pq a
    
    -- Creează o coadă de priorități dintr-o lista de tupluri
    fromList :: [(Prio, a)] -> pq a
    {-  foldr, when applied to a binary operator, a starting value (typically
     the right-identity of the operator), and a list, reduces the list using the 
     binary operator, from right to left -}
    fromList = foldr insert empty

    toList :: pq a -> [(Prio, a)]
    toList pQueue
      | isEmpty pQueue = []

      {- The fromJust function extracts the element out 
      of a Just and throws an error if its argument is Nothing. -}
      | otherwise      = (fromJust $ top pQueue) : (toList $ pop pQueue)

    size :: pq a -> Int
    -- converteste in lista si aflam lungimea
    size = length . toList -- Pentru  6

test1OK = True

-- Test 1

check1 :: TestData
check1 = test 1 $ testManually "Priority Queue Class" 1.5 test1OK -- 1.5p

-------------------------------------------------------------------------------

{-
    2.(2.0p) Definiți tipul ListPQ care reprezintă o coadă de priorități ca pe o
    listă de elemente. Includeți ListPQ în clasa PQueue.
-}

newtype ListPQ a = LPQ [(Prio, a)] deriving Show
-- Includem ListPQ în clasa PQueue
instance (Ord a) => PQueue ListPQ a where
     -- Construiește tipul ListPQ o coadă de priorități goală
    empty = LPQ []
    -- inseram un element in coada de prioritati
    insert elem (LPQ pq)
      | null pq                   = LPQ [elem]
      -- compar elementul cu capul cozii, daca este mai mare il adaug in cap
      -- pq este o lista de perechi
      | fst elem >= fst (head pq) = LPQ $ elem : pq
      -- altfel adaugam elmentul dupa capul listei
      -- iau coada cozii, o fac coada, dupa care adaug elementul, dupa care transform in lista, dupa care concatenezcu capul cozii
      -- si in final o transform in coada
      | otherwise                 = LPQ $ head pq : toList (insert elem $ LPQ $ tail pq)

    top (LPQ pq)
    -- daca este caoda vida, returnez nimic
      | null pq    = Nothing
      -- altfel, returnez doar capul listei
      | otherwise  = Just $ head pq
    
      -- extrag coada din coada pq, dupa care o transform in tipul LPQ
    pop (LPQ pq) = LPQ (tail pq) 
    -- RETURNEZ null daca coada este vida
    isEmpty (LPQ pq) = null pq

-- Test 2

listPQInt :: ListPQ Int
listPQInt = fromList elemsInt

listPQStr :: ListPQ String
listPQStr = fromList elemsStr

check2 :: TestData
check2 = tests 2 2.0 -- 2.0p
            [
              testVal "ListPQ Int check" 1.5 (reverse $ sort elemsInt) $ toList listPQInt,
              testVal "ListPQ Str check" 1.5 (reverse $ sort elemsStr) $ toList listPQStr
            ]

{-
    LeftistPQ reprezintă o coadă de priorități ca pe un arbore binar.
    Fiecare nod va conține elementele:
    * Prioritatea
    * Rank-ul
    * Subarborele stang
    * Subarborele drept
    Referință - pentru mai multe detalii despre construcție: http://typeocaml.com/2015/03/12/heap-leftist-tree/  
    Vizualizare: https://www.cs.usfca.edu/~galles/visualization/LeftistHeap.html
-}

type Rank = Int

data LeftistPQ a = Empty { rank :: Rank } |
                   Node { rank :: Rank, nodeVal :: (Prio, a), left :: LeftistPQ a, right :: LeftistPQ a }

{-
    3.(2.5p) Definiți operația de "merge" care primește 2 parametri de tipul LeftistPQ și intoarce
    un nou LeftistPQ obținut prin combinare.
    Cazuri de tratat:
    * Dacă unul dintre noduri este Empty
    * Dacă ambele noduri sunt Empty
    * Dacă nodurile nu sunt Empty
    Trebuie definită și operația inorder pentru parcurgerea arborelui - este folosit la validare
-}

merge :: LeftistPQ a -> LeftistPQ a -> LeftistPQ a
-- tratam cazul cand nodul din dreapta este Empty
merge (Empty _) tree = tree
-- tratam cazul cand nodul din stanga este Empty
merge tree (Empty _) = tree
-- folosim constructorul Node pentrua forma un nou arbore cu ranguri
merge tree1@(Node r1 (p1, val1) left1 right1) tree2@(Node _ (p2, _) _ _)
   -- daca prioritatea p1  din arborele stg e mai mica ca p2, arborele drept, aplicam merge recursiv pe arborii tree2 si tree1
  | p1 <= p2                      = merge tree2 tree1
  -- rotim arborele, subarborele stang sa fie mai mic ca subarborele drept
  -- dc p1 > p2, ne ducem pe arborele din stg, verifica rangurile 
  -- si dc rang arbore stg e mai mare, ramane asa, altfel rotim
  | rank left1 >= rank mergedTree = (Node (1 + rank mergedTree) (p1, val1) left1 mergedTree)
  | otherwise                     = (Node (1 + rank left1) (p1, val1) mergedTree left1)
    where
        -- altfel facem merge pe cel din dreapta cu tree2
      mergedTree = merge right1 tree2
-- prio reprezinta prioritatea,
-- parcurgem in inordine, subarborele stang, radacina, apoi subarborele drept
inorder :: LeftistPQ a -> [(Prio, a)]
inorder (Empty _) = []
inorder (Node r val leftTree rightTree) = inorder leftTree ++ [val] ++ inorder rightTree

-- Test 3

check3 :: TestData
check3 = tests 3 2.5
          [
            testVal "Inorder Merge Empty NotEmpty" 0.25 [(3,4)] $ inorder $ merge emptyNode node1,
            testVal "Inorder Merge NotEmpty Empty" 0.25 [(3,4)] $ inorder $ merge node1 emptyNode,
            testVal "Inorder Merge NotEmpty NotEmpty" 0.25 [(3,4), (5,10)] $ inorder $ merge node1 node2,
            testVal "Inorder Merge" 0.25 [(3,4), (5,10), (10,20), (4,10)] $ inorder $ merge node4 $ merge node3 $ merge node1 node2
          ]
        where
          emptyNode = Empty 0
          node1 = Node 1 (3, 4) emptyNode emptyNode
          node2 = Node 1 (5, 10) emptyNode emptyNode
          node3 = Node 1 (10, 20) emptyNode emptyNode
          node4 = Node 1 (4, 10) emptyNode emptyNode

          {-
           - Inorder Merge Test:
           - Primul merge:
           -      (5,10)
           -       / \ 
           -      /   \
           -     /     \
           -  (3,4)   ---
           -
           -
           - Al doilea merge:
           -        (10,20)
           -         /  \
           -        /    \
           -       /      \
           -    (5,10)    ---
           -     /
           -    /
           -  (3,4)
           -
           -  Al treilea merge:
           -         (10,20)
           -           /  \
           -          /    \
           -         /      \
           -      (5,10)    (4,10)
           -       /
           -      /
           -     /
           -   (3,4)
           -}
{-
    4.(1.5p) Includeți LeftistPQ în PQueue
-}

instance (Ord a) => PQueue LeftistPQ a where
  -- aceleasi operatii, dar pe LeftistPQ, ADICA ARBORELE
    empty = Empty 0

    isEmpty (Empty _) = True
    isEmpty (Node _ _ _ _) = False
    -- facem merge prin care adaug un nou nod cu rangul 1
    -- empy empty sunt frunzele cu rangul 0
    insert elem = merge (Node 1 elem empty empty)

    top pq
      | isEmpty pq = Nothing
      -- Get the value from a Node.
      | otherwise  = Just $ nodeVal pq
    -- facem pop, facand merge pe left si right pq
    pop pq = merge (left pq) (right pq)

-- Test 4

leftistPQInt :: LeftistPQ Int
leftistPQInt = fromList elemsInt

leftistPQStr :: LeftistPQ String
leftistPQStr = fromList elemsStr

check4 :: TestData
check4 = tests 4 1.5 -- 1.5p
          [
            testVal "LeftistPQ toList" 0.75 (reverse $ sort elemsInt) $ toList leftistPQInt,
            testVal "LeftistPQ toList" 0.75 (reverse $ sort elemsStr) $ toList leftistPQStr
          ]

{-
    5.(1.0p) Definiți funcția convert care face conversia intre cele 2 tipuri de reprezentări
-}

convert :: (PQueue pq1 a, PQueue pq2 a) => pq1 a -> pq2 a
-- convertim folosinf functiile de mai sus, din lista in coada si din coada in lista, compunand functiile from si to
convert = fromList . toList 

-- Test 5

check5 :: TestData
check5 = tests 5 1.0 -- 1.0p
          [
            testVal "Convert ListPQInt to LeftistPQInt" 1 (toList listPQInt) $ toList convertedLeftistPQInt,
            testVal "Convert ListPQStr to LeftistPQStr" 1 (toList listPQStr) $ toList convertedLeftistPQStr,
            testVal "Convert LeftistPQInt to ListPQInt" 1 (toList leftistPQInt) $ toList convertedListPQInt,
            testVal "Convert LeftistPQStr to ListPQStr" 1 (toList leftistPQStr) $ toList convertedListPQStr
          ]
        where
          convertedLeftistPQInt :: LeftistPQ Int
          convertedLeftistPQInt = convert listPQInt

          convertedLeftistPQStr :: LeftistPQ String
          convertedLeftistPQStr = convert listPQStr

          convertedListPQInt :: ListPQ Int
          convertedListPQInt = convert leftistPQInt

          convertedListPQStr :: LeftistPQ String
          convertedListPQStr = convert leftistPQStr


{-
    6.(1.5p) Adăugați o nouă funcție "size" în clasa PQueue care întoarce numărul de elemente din coadă
    Atenție: Trebuie să fie definită implicit în PQueue
-}

-- Test 6

check6 :: TestData
check6 = tests 6 1.5 -- 1.5p
          [
            testVal "Size ListPQ Int" 0.5 (size listPQInt) refSize,
            testVal "Size LeftistPQ Int" 0.5 (size leftistPQInt) refSize
          ]
        where
          refSize = length elemsInt

{-
    7.(BONUS 2.0p)  Adăugați tipurile ListPQ și LeftistPQ în clasa MyFoldable
        Funcția f primește drept parametri: o valoare din coadă (al doilea element din tuplu)
    și acumulatorul.
        Pentru ListPQ foldr' ar trebui să aibă același comportament ca foldr.
        Pentru LeftistPQ foldr' ar trebui să parcurgă arborele dreapta, rădăcină, stânga.
        

    Reminder:
        :t foldr
        foldr :: (a -> b -> b) -> b -> [a] -> b 

        În Haskell 8.x.x tipul arată în felul următor
        :t foldr
        foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b

        Clasa Foldable caracterizează tipurile care pot fi "reduse" la o anumită valoare utilizând operații specifice (foldr, foldl).
        Foldable t este o constrângere de tip, iar "t a" este un container care conține valori de tipul a.

        Mai multe informații: https://wiki.haskell.org/Foldable_and_Traversable
-}

-- Dacă doriți, puteți modifica contextul tipului lui foldr'
class MyFoldable f where
    foldr' :: (a -> b -> b) -> b -> f a -> b

instance MyFoldable ListPQ where
    foldr' = undefined

instance MyFoldable LeftistPQ where
    foldr' = undefined

-- Test 7

check7 :: TestData
check7 = tests 7 2.0 -- 2.0p
          [
            testVal "MyFoldable ListPQ Int" 0.5 0 $ foldr' fInt 0 listPQInt,
            testVal "MyFoldable ListPQ Str" 0.5 "IAPCPPPALFAAA" $ foldr' fStr "" listPQStr,
            testVal "MyFoldable LeftistPQ Int" 0.5 0 $ foldr' fInt 0 leftistPQInt,
            testVal "MyFoldable LeftistPQ Str" 0.5 "AAPCPAIALFAPP" $ foldr' fStr "" leftistPQStr
          ]
        where
          fStr = (++)
          fInt = (*)

          {-
           -     Leftist PQ (Rank, Priority, Value)
           -           (2,        20,     "IA")
           -             /                    \
           -            /                      \
           -           /                        \
           -       (2, 10, "PC")                (1,    5,   "PP")
           -         /        \                     /         \
           -        /          \                   /           \
           -       /            \                 /             \
           -    (1, -3, "AA")   (1, 2, "PA")    (1, 1, "LFA")   ---
           -     /      \            /  \           /  \
           -    /        \          /    \         /    \
           -   /          \        /      \       /      \
           - ---          ---     ---     ---    ---     ---
           -
           -}


{-
    8.(BONUS 1.0p)  Adăugați tipurile ListPQ și LeftistPQ în clasa MyFunctor
       Funcția f primește ca parametru o valoare din coadă (al doilea element din tuplu)
-}

class MyFunctor f where
    fmap' :: (Ord a, Ord b) => ((Prio, a)  -> (Prio, b)) -> f a -> f b

instance MyFunctor ListPQ where
    fmap' = undefined

instance MyFunctor LeftistPQ where
    fmap' = undefined

-- Test 8

check8 :: TestData
check8 = tests 8 1.0 -- 1.0p
          [
            testVal "MyFunctor ListPQ Int" 0.5  refInt $ toList $ fmap' fInt listPQInt,
            testVal "MyFunctor ListPQ Str" 0.5 refStr $ toList $ fmap' fStr listPQStr,
            testVal "MyFunctor LeftistPQ Int" 0.5  refInt $ toList $ fmap' fInt leftistPQInt,
            testVal "MyFunctor LeftistPQ Str" 0.5 refStr $ toList $ fmap' fStr leftistPQStr
          ]
        where
          fInt (x, y) = (x - 10, y + 100)
          fStr (x, y) = (x + 10, y ++ "42")

          refInt = reverse $ sort $ map fInt elemsInt
          refStr = reverse $ sort $ map fStr elemsStr

{-
    9.(BONUS 2.0p) Adăugați LeftistPQ în clasa Show
    Va trebui ca arborele să fie afișat în modul următor:
    "--" x nivel în arbore {valoare din nod}
    Dacă nodul este Empty atunci se va afișa în loc de {valoare din nod} "empty"
    Ex: Node _ (3,4) {Node _ (4,5) Empty Empty} {Node _ (5,6) {Node _ (6,7) Empty Empty} Empty} -- nu ne interesează rankul
    --(3,4)
    ----(4,5)
    ------empty
    ------empty
    ----(5,6)
    ------(6,7)
    --------empty
    --------empty
    ------empty <-- și aici este newline la final

    Hint: Parcurgere preordine
-}

instance (Show a) => Show (LeftistPQ a) where
    show = undefined

-- Test 9

check9 :: TestData
check9 = tests 9 2.0 -- 2.0p
          [
            testVal "Show LeftistPQ Str" 1.0 refLeftistPQStr $ show leftistPQStr,
            testVal "Show LeftistPQ Int" 1.0 refLeftistPQInt $ show leftistPQInt
          ]
        where
          refLeftistPQStr = "--(20,\"IA\")\n\
                           \----(10,\"PC\")\n\
                           \------(-3,\"AA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,\"PA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,\"PP\")\n\
                           \------(1,\"LFA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

          refLeftistPQInt = "--(20,30023)\n\
                           \----(10,30)\n\
                           \------(-3,500)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,100)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,20)\n\
                           \------(1,1000)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

check = quickCheck [check1, check2, check3, check4, check5, check6, check7, check8, check9]


{- ========================================================================================= -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PQueue where

import Data.Maybe
import Data.List (sort)
import TestPP

{-
    Următoarele exerciții vor avea drept scop implementarea unei mici biblioteci
    pentru o coadă de priorități (mai specific, un max priority queue - "primul" element
    are prioritatea cea mai mare).
    Coada de priorități va fi reprezentată folosind o listă sau un arbore binar.
    Biblioteca noastră va defini o reprezentare generală pentru coada de priorități,
    precum și funcții care operează pe aceasta.
-}

 ----------------------------------------------------------------------------------------------------
{- Setup Testare -}

valsInt = [20, 100, 30, 500, 1000, 30023, 513]
valsStr = ["PP", "PA", "PC", "AA", "LFA", "IA", "ML"]
prios = [5, 2, 10, -3, 1, 20]
elemsInt = zip prios valsInt
elemsStr = zip prios valsStr

-- Considerăm că un element din coadă este reprezentat de un tuplu care va conține:
-- * prioritatea
-- * valoarea
type Prio = Int

{-
    1.(1.5p) Analizați clasa PQueue definită mai jos și scrieți implementările
    implicite pentru funcțiile din această clasă:
    * fromList
    * toList

    Clasa PQueue definește interfața pentru toate structurile de coada de priorități
    pe care le vom implementa mai jos.
-}

class (Ord a) => PQueue pq a where

    -- Construiește o coadă de priorități goală
    empty :: pq a

    -- Verifică dacă coada este goală
    isEmpty :: pq a -> Bool

    -- Inserează elem in coada cu priorități
    insert :: (Prio, a) -> pq a -> pq a

    -- Întoarce primul element din coada de priorități
    top :: pq a -> Maybe (Prio, a)

    -- Șterge primul element din coada de priorități
    -- Dacă coada nu are elemente se va returna coada goală
    pop :: pq a -> pq a
    
    -- Creează o coadă de priorități dintr-o lista de tupluri
    fromList :: [(Prio, a)] -> pq a
    fromList = foldr insert empty

    toList :: pq a -> [(Prio, a)]
    toList pQueue
      | isEmpty pQueue = []
      | otherwise      = (fromJust $ top pQueue) : (toList $ pop pQueue)

    size :: pq a -> Int
    size = length . toList -- Pentru BONUS 6

test1OK = True

-- Test 1

check1 :: TestData
check1 = test 1 $ testManually "Priority Queue Class" 1.5 test1OK -- 1.5p

-------------------------------------------------------------------------------

{-
    2.(2.0p) Definiți tipul ListPQ care reprezintă o coadă de priorități ca pe o
    listă de elemente. Includeți ListPQ în clasa PQueue.
-}

newtype ListPQ a = LPQ [(Prio, a)] deriving Show

instance (Ord a) => PQueue ListPQ a where
    empty = LPQ []

    insert elem (LPQ pq)
      | null pq                   = LPQ [elem]
      | fst elem >= fst (head pq) = LPQ $ elem : pq
      | otherwise                 = LPQ $ head pq : toList (insert elem $ LPQ $ tail pq)

    top (LPQ pq)
      | null pq    = Nothing
      | otherwise  = Just $ head pq

    pop (LPQ pq) = LPQ (tail pq) 

    isEmpty (LPQ pq) = null pq

-- Test 2

listPQInt :: ListPQ Int
listPQInt = fromList elemsInt

listPQStr :: ListPQ String
listPQStr = fromList elemsStr

check2 :: TestData
check2 = tests 2 2.0 -- 2.0p
            [
              testVal "ListPQ Int check" 1.5 (reverse $ sort elemsInt) $ toList listPQInt,
              testVal "ListPQ Str check" 1.5 (reverse $ sort elemsStr) $ toList listPQStr
            ]

{-
    LeftistPQ reprezintă o coadă de priorități ca pe un arbore binar.
    Fiecare nod va conține elementele:
    * Prioritatea
    * Rank-ul
    * Subarborele stang
    * Subarborele drept
    Referință - pentru mai multe detalii despre construcție: http://typeocaml.com/2015/03/12/heap-leftist-tree/  
    Vizualizare: https://www.cs.usfca.edu/~galles/visualization/LeftistHeap.html
-}

type Rank = Int

data LeftistPQ a = Empty { rank :: Rank } |
                   Node { rank :: Rank, nodeVal :: (Prio, a), left :: LeftistPQ a, right :: LeftistPQ a }

{-
    3.(2.5p) Definiți operația de "merge" care primește 2 parametri de tipul LeftistPQ și intoarce
    un nou LeftistPQ obținut prin combinare.
    Cazuri de tratat:
    * Dacă unul dintre noduri este Empty
    * Dacă ambele noduri sunt Empty
    * Dacă nodurile nu sunt Empty
    Trebuie definită și operația inorder pentru parcurgerea arborelui - este folosit la validare
-}

merge :: LeftistPQ a -> LeftistPQ a -> LeftistPQ a
merge (Empty _) tree = tree
merge tree (Empty _) = tree
merge tree1@(Node r1 (p1, val1) left1 right1) tree2@(Node _ (p2, _) _ _)
  | p1 <= p2                      = merge tree2 tree1
  | rank left1 >= rank mergedTree = (Node (1 + rank mergedTree) (p1, val1) left1 mergedTree)
  | otherwise                     = (Node (1 + rank left1) (p1, val1) mergedTree left1)
    where
      mergedTree = merge right1 tree2

inorder :: LeftistPQ a -> [(Prio, a)]
inorder (Empty _) = []
inorder (Node r val leftTree rightTree) = inorder leftTree ++ [val] ++ inorder rightTree

-- Test 3

check3 :: TestData
check3 = tests 3 2.5
          [
            testVal "Inorder Merge Empty NotEmpty" 0.25 [(3,4)] $ inorder $ merge emptyNode node1,
            testVal "Inorder Merge NotEmpty Empty" 0.25 [(3,4)] $ inorder $ merge node1 emptyNode,
            testVal "Inorder Merge NotEmpty NotEmpty" 0.25 [(3,4), (5,10)] $ inorder $ merge node1 node2,
            testVal "Inorder Merge" 0.25 [(3,4), (5,10), (10,20), (4,10)] $ inorder $ merge node4 $ merge node3 $ merge node1 node2
          ]
        where
          emptyNode = Empty 0
          node1 = Node 1 (3, 4) emptyNode emptyNode
          node2 = Node 1 (5, 10) emptyNode emptyNode
          node3 = Node 1 (10, 20) emptyNode emptyNode
          node4 = Node 1 (4, 10) emptyNode emptyNode

          {-
           - Inorder Merge Test:
           - Primul merge:
           -      (5,10)
           -       / \ 
           -      /   \
           -     /     \
           -  (3,4)   ---
           -
           -
           - Al doilea merge:
           -        (10,20)
           -         /  \
           -        /    \
           -       /      \
           -    (5,10)    ---
           -     /
           -    /
           -  (3,4)
           -
           -  Al treilea merge:
           -         (10,20)
           -           /  \
           -          /    \
           -         /      \
           -      (5,10)    (4,10)
           -       /
           -      /
           -     /
           -   (3,4)
           -}
{-
    4.(1.5p) Includeți LeftistPQ în PQueue
-}

instance (Ord a) => PQueue LeftistPQ a where

    empty = Empty 0

    isEmpty (Empty _) = True
    isEmpty _ = False
    
    insert elem = merge (Node 1 elem empty empty)

    top pq
      | isEmpty pq = Nothing
      | otherwise  = Just $ nodeVal pq

    pop pq = merge (left pq) (right pq)

-- Test 4

leftistPQInt :: LeftistPQ Int
leftistPQInt = fromList elemsInt

leftistPQStr :: LeftistPQ String
leftistPQStr = fromList elemsStr

check4 :: TestData
check4 = tests 4 1.5 -- 1.5p
          [
            testVal "LeftistPQ toList" 0.75 (reverse $ sort elemsInt) $ toList leftistPQInt,
            testVal "LeftistPQ toList" 0.75 (reverse $ sort elemsStr) $ toList leftistPQStr
          ]

{-
    5.(1.0p) Definiți funcția convert care face conversia intre cele 2 tipuri de reprezentări
-}

convert :: (PQueue pq1 a, PQueue pq2 a) => pq1 a -> pq2 a
convert = fromList . toList 

-- Test 5

check5 :: TestData
check5 = tests 5 1.0 -- 1.0p
          [
            testVal "Convert ListPQInt to LeftistPQInt" 1 (toList listPQInt) $ toList convertedLeftistPQInt,
            testVal "Convert ListPQStr to LeftistPQStr" 1 (toList listPQStr) $ toList convertedLeftistPQStr,
            testVal "Convert LeftistPQInt to ListPQInt" 1 (toList leftistPQInt) $ toList convertedListPQInt,
            testVal "Convert LeftistPQStr to ListPQStr" 1 (toList leftistPQStr) $ toList convertedListPQStr
          ]
        where
          convertedLeftistPQInt :: LeftistPQ Int
          convertedLeftistPQInt = convert listPQInt

          convertedLeftistPQStr :: LeftistPQ String
          convertedLeftistPQStr = convert listPQStr

          convertedListPQInt :: ListPQ Int
          convertedListPQInt = convert leftistPQInt

          convertedListPQStr :: LeftistPQ String
          convertedListPQStr = convert leftistPQStr


{-
    6.(1.5p) Adăugați o nouă funcție "size" în clasa PQueue care întoarce numărul de elemente din coadă
    Atenție: Trebuie să fie definită implicit în PQueue
-}

-- Test 6

check6 :: TestData
check6 = tests 6 1.5 -- 1.5p
          [
            testVal "Size ListPQ Int" 0.5 (size listPQInt) refSize,
            testVal "Size LeftistPQ Int" 0.5 (size leftistPQInt) refSize
          ]
        where
          refSize = length elemsInt

{-
    7.(BONUS 2.0p)  Adăugați tipurile ListPQ și LeftistPQ în clasa MyFoldable
        Funcția f primește drept parametri: o valoare din coadă (al doilea element din tuplu)
    și acumulatorul.
        Pentru ListPQ foldr' ar trebui să aibă același comportament ca foldr.
        Pentru LeftistPQ foldr' ar trebui să parcurgă arborele dreapta, rădăcină, stânga.
        

    Reminder:
        :t foldr
        foldr :: (a -> b -> b) -> b -> [a] -> b 

        În Haskell 8.x.x tipul arată în felul următor
        :t foldr
        foldr :: (Foldable t) => (a -> b -> b) -> b -> t a -> b

        Clasa Foldable caracterizează tipurile care pot fi "reduse" la o anumită valoare utilizând operații specifice (foldr, foldl).
        Foldable t este o constrângere de tip, iar "t a" este un container care conține valori de tipul a.

        Mai multe informații: https://wiki.haskell.org/Foldable_and_Traversable
-}

class MyFoldable f where
    foldr' :: (a -> b -> b) -> b -> f a -> b

instance MyFoldable ListPQ where
    foldr' f acc (LPQ pq) = foldr (\ elem -> f $ snd elem) acc pq

instance MyFoldable LeftistPQ where
    foldr' f acc = foldr (\ elem -> f $ snd elem) acc . inorder

-- Test 7

check7 :: TestData
check7 = tests 7 2.0 -- 2.0p
          [
            testVal "MyFoldable ListPQ Int" 0.5 0 $ foldr' fInt 0 listPQInt,
            testVal "MyFoldable ListPQ Str" 0.5 "IAPCPPPALFAAA" $ foldr' fStr "" listPQStr,
            testVal "MyFoldable LeftistPQ Int" 0.5 0 $ foldr' fInt 0 leftistPQInt,
            testVal "MyFoldable LeftistPQ Str" 0.5 "AAPCPAIALFAPP" $ foldr' fStr "" leftistPQStr
          ]
        where
          fStr = (++)
          fInt = (*)

          {-
           -     Leftist PQ (Rank, Priority, Value)
           -           (2,        20,     "IA")
           -             /                    \
           -            /                      \
           -           /                        \
           -       (2, 10, "PC")                (1,    5,   "PP")
           -         /        \                     /         \
           -        /          \                   /           \
           -       /            \                 /             \
           -    (1, -3, "AA")   (1, 2, "PA")    (1, 1, "LFA")   ---
           -     /      \            /  \           /  \
           -    /        \          /    \         /    \
           -   /          \        /      \       /      \
           - ---          ---     ---     ---    ---     ---
           -
           -}


{-
    8.(BONUS 1.0p)  Adăugați tipurile ListPQ și LeftistPQ în clasa MyFunctor
       Funcția f primește ca parametru o valoare din coadă (al doilea element din tuplu)
-}

class MyFunctor f where
    fmap' :: (Ord a, Ord b) => ((Prio, a)  -> (Prio, b)) -> f a -> f b

instance MyFunctor ListPQ where
    fmap' f (LPQ pq) = fromList $  map f pq

instance MyFunctor LeftistPQ where
    fmap' f = fromList . map f . inorder

-- Test 8

check8 :: TestData
check8 = tests 8 1.0 -- 1.0p
          [
            testVal "MyFunctor ListPQ Int" 0.5  refInt $ toList $ fmap' fInt listPQInt,
            testVal "MyFunctor ListPQ Str" 0.5 refStr $ toList $ fmap' fStr listPQStr,
            testVal "MyFunctor LeftistPQ Int" 0.5  refInt $ toList $ fmap' fInt leftistPQInt,
            testVal "MyFuncotr LeftistPQ Str" 0.5 refStr $ toList $ fmap' fStr leftistPQStr
          ]
        where
          fInt (x, y) = (x - 10, y + 100)
          fStr (x, y) = (x + 10, y ++ "42")

          refInt = reverse $ sort $ map fInt elemsInt
          refStr = reverse $ sort $ map fStr elemsStr

{-
    9.(BONUS 2.0p) Adăugați LeftistPQ în clasa Show
    Va trebui ca arborele să fie afișat în modul următor:
    "--" x nivel în arbore {valoare din nod}
    Dacă nodul este Empty atunci se va afișa în loc de {valoare din nod} "empty"
    Ex: Node _ (3,4) {Node _ (4,5) Empty Empty} {Node _ (5,6) {Node _ (6,7) Empty Empty} Empty} -- nu ne interesează rankul
    --(3,4)
    ----(4,5)
    ------empty
    ------empty
    ----(5,6)
    ------(6,7)
    --------empty
    --------empty
    ------empty <-- și aici este newline la final

    Hint: Parcurgere preordine
-}

showLeftist :: (Show a) => String -> LeftistPQ a -> String
showLeftist dashes (Empty _) = dashes ++ "empty\n"
showLeftist dashes (Node _ val left right) = dashes ++ (show val) ++ "\n" ++ leftShow ++ rightShow
  where
    leftShow = (showLeftist newDashes left)
    rightShow = (showLeftist newDashes right)
    newDashes = dashes ++ "--"

instance (Show a) => Show (LeftistPQ a) where
    show = showLeftist "--"

-- Test 9

check9 :: TestData
check9 = tests 9 2.0 -- 2.0p
          [
            testVal "Show LeftistPQ Str" 1.0 refLeftistPQStr $ show leftistPQStr,
            testVal "Show LeftistPQ Int" 1.0 refLeftistPQInt $ show leftistPQInt
          ]
        where
          refLeftistPQStr = "--(20,\"IA\")\n\
                           \----(10,\"PC\")\n\
                           \------(-3,\"AA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,\"PA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,\"PP\")\n\
                           \------(1,\"LFA\")\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

          refLeftistPQInt = "--(20,30023)\n\
                           \----(10,30)\n\
                           \------(-3,500)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------(2,100)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \----(5,20)\n\
                           \------(1,1000)\n\
                           \--------empty\n\
                           \--------empty\n\
                           \------empty\n"

check = quickCheck [check1, check2, check3, check4, check5, check6, check7, check8, check9]


