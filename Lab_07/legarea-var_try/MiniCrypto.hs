module MiniCrypto where

{-
  PP, Laboratorul 7

  Laboratorul presupune implementarea unei mini-biblioteci de primitive
  criptografice: cifrări flux, cifrări bazate pe substituție (Caesar,
  Vigenere, One Time Pad).
-}

import Data.List
import Data.Word
import Data.Bits
import Data.Char
import System.Random hiding (randoms)
import TestPP

{-
Testare:
- Pentru a rula toate testele apelati functia check
> check

- Pentru a rula testele asociate exercitiului cu indicele 'i', apelati 'checki'
De exemplu, pentru a verifica exercitiul 6:
> check6 
-}

{-
  Funcții auxiliare: conversie Char-Word
-}
charToWord :: Char -> Word8
charToWord = fromIntegral . fromEnum

wordToChar :: Word8 -> Char
wordToChar = toEnum . fromIntegral
        
{-
  1. (1p)  
  Construiți funcția myCycle, care ia ca argument o listă și întoarce lista
  repetată la infinit. Ex: myCycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,..]

  Hint: Puteți defini funcția „point-free”, folosind funcții din cadrul
  modulului Data.List.
  http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html

  Observaţie: Nu folosiți în implementare funcția cycle. :-)
-}

test1 :: TestPP ()
test1 = testOne 1 $ testVal (take 42 $ myCycle xs) (take 42 $ cycle xs) "myCycle" 1
  where xs = [1,2,3,4]

myCycle :: [a] -> [a]
myCycle = concat . repeat

{-
  2. (2p)
  Construiţi o progresie aritmetică şi o progresie geometrică pornind de la 
  primul termen şi raţia în fiecare dintre cazuri.
  Ex: arithmetic 1 3 = [1,4,7,..]
      geometric  2 3 = [2,6,18,..]

  Hint: folosiţi funcţia iterate din cadrul modulului Data.List.
  http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html
-}

test2 :: TestPP ()
test2 = tests 2 2
  [ testVal (take 10 $ arithmetic initial r) [5,11,17,23,29,35,41,47,53,59] "arithmetic" 1
  , testVal (take 10 $ geometric initial q) [5,10,20,40,80,160,320,640,1280,2560] "geometric" 1
  ]
  where { initial = 5 ; r = 6 ; q = 2 }

  {-  a este o variabilă de tip (stă pentru orice fel de tip)
   restricționată (prin folosirea =>) la toate tipurile numerice (Num a). -}
arithmetic :: Num a => a -> a -> [a]
{- iterate f x returns an infinite list of repeated applications of f to x -}
arithmetic initial ratio = iterate (+ ratio) initial

geometric :: Num a => a -> a -> [a]
geometric initial ratio = iterate (* ratio) initial

{-
  3. (2p)
  Construiți o funcție care întoarce un șir infinit de numere pseudo-aleatoare,
  plecând de la o valoare „seed” întreagă. 
  
  Tipul elementelor listei va fi Word8 (numerele vor fi între 0 și 255). 
  Folosiți fromIntegral pentru a realiza conversii între tipuri numerice întregi.
  
  Folosiți funcțiile definite în modulul System.Random pentru a genera numere.
  http://www.haskell.org/ghc/docs/6.12.2/html/libraries/random-1.0.0.2/System-Random.html

  Ex: > take 10 $ randoms 42
  [38,166,220,81,67,142,213,118,105,10]

  Hint: Folosiți-vă de mkStdGen, next și (eventual) alte funcții din
  System.Random. *Nu* este necesară folosirea de funcții care întorc
  valori de tipul IO.
-}

test3 :: TestPP ()
test3 = testOne 3 $ testVal (take 10 $ randoms 42) [38,166,220,81,67,142,213,118,105,10] "randoms" 2
{- mkStdGen primeste un numar natural (seed) si
intoarce un generator de numere aleatoare.
next primeste un generator si intoarce un tuplu:
(urmatorul numar generat, noua stare a generatorului) -}
randoms :: Int -> [Word8]
{- next primeste ca parametru noua stare a generatorului, adica snd din tuplul
 (number_generated, neq_state_generator) si iteram functia next pe valoarea returnata de mkStdGen si apoi
 facem conversia in intreg pe numarul generat random. Cu tail luam primul element din lista genearata. -}
randoms seed = tail $ map (fromIntegral . fst) $ iterate (next . snd) (0, mkStdGen seed)

{-
    4. (1p)
    Implementați o funcție care convertește un element de tipul Word8 (0 - 255) într-un caracter
    alfabetic ('a' - 'z').
    
    Ex: > map wordToAlpha [0, 1, 2, 3, 26, 27, 28, 29]
    "abcdabcd"
       
    Recomandare: folosiți o clauză where sau let.
       
    Hint: Puteți folosi funcțiile 'ord' si 'chr' din Data.Char.
    https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Char.html#g:6
    
    Hint: Puteți folosi funcțiile fromIntegral, mod.
-}

test4 :: TestPP ()
test4 = testOne 4 $ testVal (map wordToAlpha [0..255]) (take 256 $ cycle ['a'..'z']) "wordToAlpha" 1

wordToAlpha :: Word8 -> Char
{- char = The toEnum method restricted to the type Char. -}
wordToAlpha x = chr $ base + idx
    where
      {- ord intoarce valoarea caracterului a in Dec din ASCII table, adica 97 -}
        base = ord 'a'  
        idx  = fromIntegral $ x `mod` 26

{-
    4. (1p)
    Implementați o funcție care generează o secvență infinită de caractere alfabetice
    pseudo-aleatoare, plecând de la o valoare "seed" întreagă.
    
    Observație: Implementați funcția „point-free”.
    Hint: Folosiți funcțiile randoms si wordToAlpha de la exercițiile anterioare.
-}

test5 :: TestPP ()
test5 = testOne 5 $ testVal (take 10 $ randomAlphaKey 42) "mkmdpmfobk" "randomAlphaKey" 1

randomAlphaKey :: Int -> String
{- Compunem functiile randoms si wordToAlpha, formand o functie noua care 
genereaza o secventa infinita de caractere alfabetice. Map primeste ca parametru noua
functie compusa, si asteapta al doilea parametru lista, fiind o functie curry -}
randomAlphaKey = map wordToAlpha . randoms

{-
  6. (3p)
  Implementați funcția tableToFunc, care primește o listă de asocieri
  (caracter-clar, caracter-criptat) și întoarce o funcție de substituție.

  Implementați funcția substCrypt, care primește o listă de asocieri
  (caracter-clar, caracter-criptat) și un
  șir de caractere și întoarce șirul de caractere criptat pe baza tabelei.

  Observație: tableToFunc va fi implementată obligatoriu utilizând o clauză
  where/let, cu pattern matching.

  Observație: substCrypt va fi implementată obligatoriu „point-free” (nu va
  avea parametri expliciți), folosind funcționale și/sau clauze let/where.
-}

test6 :: TestPP ()
test6 = tests 6 3
  [ testVal (substCrypt rot13Table str) cryptstr "substCrypt" 2
  , testVal (tableToFunc rot13Table 'd') 'q' "tableToFunc" 1
  ]
  where
   str      = "thequickbrownfoxjumpsoverthelazydog"
   cryptstr = "gurdhvpxoebjasbkwhzcfbiregurynmlqbt"

rot13Table = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),
              ('f','s'),('g','t'),('h','u'),('i','v'),('j','w'),
              ('k','x'),('l','y'),('m','z'),('n','a'),('o','b'),
              ('p','c'),('q','d'),('r','e'),('s','f'),('t','g'),
              ('u','h'),('v','i'),('w','j'),('x','k'),('y','l'),
              ('z','m')]   

tableToFunc :: [(Char, Char)] -> Char -> Char
tableToFunc t key = value
{- filter, applied to a predicate and a list, returns the list
 of those elements that satisfy the predicate; -}
    where
      {- intoarcem lista cu perechile din tabela care satisfac predicatul.
      (primul element din pereche care au cheia key primita ca parametru).
      t este tabela primita ca parametru.
      Aplicam filter pe perechile tabelei si predicatul respectiv. -}
    (_, value) : _ = filter ((key ==) . fst) t
        
substCrypt :: [(Char, Char)] -> String -> String
{- facem compunere de functii intre map si tableToFunc -}
substCrypt = map . tableToFunc

{-
    7. (Bonus - 1p)
    
    Implementați funcția getRotTable, care produce o listă de asocieri 
    (caracter-clar, caracter-criptat). Funcția primește un parametru 'offset' și 
    va construi o asociere între fiecare caracter alfabetic (litere mici) și 
    caracterul aflat după următoarele 'offset' poziții.
    
    Ex: > getRotTable 1 
    [('a','b'),('b','c'),('c','d'),('d','e'),('e','f'),('f','g'),('g','h'),
     ('h','i'),('i','j'),('j','k'),('k','l'),('l','m'),('m','n'),('n','o'),
     ('o','p'),('p','q'),('q','r'),('r','s'),('s','t'),('t','u'),('u','v'),
     ('v','w'),('w','x'),('x','y'),('y','z'),('z','a')]
    
    Ex: > getRotTable 13 
    [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),
     ('h','u'),('i','v'),('j','w'),('k','x'),('l','y'),('m','z'),('n','a'),
     ('o','b'),('p','c'),('q','d'),('r','e'),('s','f'),('t','g'),('u','h'),
     ('v','i'),('w','j'),('x','k'),('y','l'),('z','m')]    
    
    Hint: Puteți să folosiți zip sau zipWith
    http://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#g:20
    
    Hint: Pentru a genera lista cu toate caracterele din alfabet, 
    puteți folosi expresia: ['a'..'z']
-}
test7 :: TestPP ()
test7 = tests 7 1
  [ testVal
    (genRotTable 1)
    [('a','b'),('b','c'),('c','d'),('d','e'),('e','f'),('f','g'),('g','h'),('h','i'),
         ('i','j'),('j','k'),('k','l'),('l','m'),('m','n'),('n','o'),('o','p'),('p','q'),
         ('q','r'),('r','s'),('s','t'),('t','u'),('u','v'),('v','w'),('w','x'),('x','y'),
         ('y','z'),('z','a')]
    "genRotTable 1" 0.5
  , testVal
    (genRotTable 13)
    [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),('h','u'),
        ('i','v'),('j','w'),('k','x'),('l','y'),('m','z'),('n','a'),('o','b'),('p','c'),
        ('q','d'),('r','e'),('s','f'),('t','g'),('u','h'),('v','i'),('w','j'),('x','k'),
        ('y','l'),('z','m')]
    "genRotTable 2" 0.5
  ]
genRotTable :: Int -> [(Char, Char)]
genRotTable = undefined
                
{-  8. (Bonus - 1p)
    Ne propunem să implementăm o funcție de criptare numită encryptVigenere.
    Mai multe despre aceasta tehnică de criptare:
    http://practicalcryptography.com/ciphers/classical-era/vigenere-gronsfeld-and-autokey/
    
    Funcția encryptVigenere primește următoarele argumente: șirul de caractere alfabetice 
    care trebuie criptat (plaintext) și o cheie secreta (secret-key).
        
    Fie `xi`, indexul caracterului alfabetic de pe pozitia 'i' din sirul de caractere original.
    Fie `yi`, indexul caracterului alfabetic de pe pozitia 'i' din cheia secreta generată la pasul anterior.
    
    Caracterul criptat de pe pozitia 'i' va putea fi obținut alegând cel de-al `xi` caracter
    din alfabet, dacă alfabetul ar fi rotit cu `yi` poziții. (vom considera alfabetul indexat de la 0)
    
    Ex:
    plaintext: 'def'
    secretkey: 'bcd'
    
    Pentru prima poziție din șir: 
    `xi` = 'd' (caracterul cu indexul 3 din alfabet)
    `yi` = 'b' (caracterul cu indexul 1 din alfabet)
    Alfabetul rotit cu 1 pozitie: "bcdefghijklmnopqrstuvwxyza"
    Prin urmare, primul caracter criptat va fi: 'e' (al 3-lea element din alfabetul rotit)
    Daca cheia are o lungime mai mică decat șirul care trebuie criptat, 
    va fi duplicată pană va ajunge la aceeași dimensiune.
    
    > encryptVigenere "attackatdawn" "lemon"
    "lxfopvefrnhr"    
-}
test8 :: TestPP ()
test8 = tests 8 1
  [ testVal
    (encryptVigenere "attackatdawn" "lemon")
    "lxfopvefrnhr"
    "encryptVigenere 1" 0.5
  , testVal
    (encryptVigenere "theturtlemoves" "bcd")
    "ujhuwuunhnqyfu"
    "encryptVigenere 2" 0.5
  ]
encryptVigenere :: String -> String -> String
encryptVigenere = undefined
{- 9, (Bonus - 2p)
   Funcția xorNumbers face xor între două numere întregi.

   Exemplu:
   xorNumbers 10 3 = 9

   Explicație:
   Numar -> Reprezentare Binară
   10    -> 0 0 0 0 1 0 1 0 
   3     -> 0 0 0 0 0 0 1 1 
   --------------------------(xor)
   9     -> 0 0 0 0 1 0 0 1 

   Funcția xorStrings face xor între două șiruri de caractere.
   Pentru fiecare poziție i, xorStrings face xor între caracterele aflate
   pe poziția i în cele 2 șiruri
   În cazul în care lungimile sunt diferite, șirul mai scurt se extinde
   (prin concatenare cu el însuși) până la lungimea celuilalt șir.

   Exemplu:
   xorStrings "zz" "aa" = "\ESC\ESC"
   
   Explicație:
   Șir         ->   Valoare
   "zz"        ->   '0x7a' '0x7a'
   "aa"        ->   '0x61' '0x61'
   ------------------------------(xor)
   "\ESC\ESC"  ->   '0x1b' '0x1b'


   Șir         ->   Valoare
   "zz"        ->   122 122
   "aa"        ->   97  97
   ------------------------------(xor)
   "\ESC\ESC"  ->   27  27
-}


xorNumbers :: Int -> Int -> Int                                                 
xorNumbers = undefined


xorStrings :: [Char] -> [Char] -> [Char]                                        
xorStrings = undefined


test9 :: TestPP ()
test9 = tests 9 2
  [ testVal
    (xorStrings "attackatdawn" "zzzzzzzzzzzz")
    "\ESC\SO\SO\ESC\EM\DC1\ESC\SO\RS\ESC\r\DC4"
    "xorStrings 1" 1
  , testVal
    (xorStrings (xorStrings "theturtlemoves" "cccccaaaaaabbb")"cccccaaaaaabbb")
    "theturtlemoves"
    "xorStrings 2" 1
  ]
{- 10, (Bonus - 1p)
   Funcția rollingXor va implementa același comportament ca xorStrings
   singura diferență fiind că șirurile pot avea lungimi diferite.

   Exemplu:
   rollingXor "zb" "aaa" = "\ESC\CAN\ESC"
   
   Explicație:
   "zb" se extinde la "zbz" si se face xorStrings între "zbz" si "aaa".
-}


rollingXor :: [Char] -> [Char] -> [Char]                                        
rollingXor = undefined


test10 :: TestPP ()
test10 = tests 10 1
  [ testVal
    (rollingXor "bcd" "theturtlemoves")
    "\SYN\v\SOH\SYN\SYN\SYN\SYN\SI\SOH\SI\f\DC2\a\DLE"
    "rollingXor 1" 0.5
  , testVal
    (rollingXor "theturtlemoves" "bcd")
    "\SYN\v\SOH\SYN\SYN\SYN\SYN\SI\SOH\SI\f\DC2\a\DLE"
    "rot13Table 2" 0.5
  ]
{-
Helpers for testing :)
-}
allTests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
check = runTestPP $ sequence_ allTests
[check1, check2, check3, check4, check5, check6, check7, check8, check9, check10] = map runTestPP allTests



{- ======================================================================================================== -}

{-
{-
    7. (Bonus - 2p)
    
    Implementați funcția getRotTable, care produce o listă de asocieri 
    (caracter-clar, caracter-criptat). Funcția primește un parametru 'offset' și 
    va construi o asociere între fiecare caracter alfabetic (litere mici) și 
    caracterul aflat după următoarele 'offset' poziții.
    
    Ex: > getRotTable 1 
    [('a','b'),('b','c'),('c','d'),('d','e'),('e','f'),('f','g'),('g','h'),
     ('h','i'),('i','j'),('j','k'),('k','l'),('l','m'),('m','n'),('n','o'),
     ('o','p'),('p','q'),('q','r'),('r','s'),('s','t'),('t','u'),('u','v'),
     ('v','w'),('w','x'),('x','y'),('y','z'),('z','a')]
    
    Ex: > getRotTable 13 
    [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),
     ('h','u'),('i','v'),('j','w'),('k','x'),('l','y'),('m','z'),('n','a'),
     ('o','b'),('p','c'),('q','d'),('r','e'),('s','f'),('t','g'),('u','h'),
     ('v','i'),('w','j'),('x','k'),('y','l'),('z','m')]    
    
    Hint: Puteți să folosiți zip sau zipWith
    http://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#g:20
    
    Hint: Pentru a genera lista cu toate caracterele din alfabet, 
    puteți folosi expresia: ['a'..'z']
-}

test7 :: TestPP ()
test7 = tests 7 2
  [ testVal
    (genRotTable 1)
    [('a','b'),('b','c'),('c','d'),('d','e'),('e','f'),('f','g'),('g','h'),('h','i'),
         ('i','j'),('j','k'),('k','l'),('l','m'),('m','n'),('n','o'),('o','p'),('p','q'),
         ('q','r'),('r','s'),('s','t'),('t','u'),('u','v'),('v','w'),('w','x'),('x','y'),
         ('y','z'),('z','a')]
    "genRotTable 1" 1
  , testVal
    (genRotTable 13)
    [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),('h','u'),
        ('i','v'),('j','w'),('k','x'),('l','y'),('m','z'),('n','a'),('o','b'),('p','c'),
        ('q','d'),('r','e'),('s','f'),('t','g'),('u','h'),('v','i'),('w','j'),('x','k'),
        ('y','l'),('z','m')]
    "genRotTable 2" 1
  ]

genRotTable :: Int -> [(Char, Char)]
genRotTable offset = zip alfa new_seq
    where
        alfa    = ['a'..'z']
        new_seq = drop offset $ myCycle alfa
                
{-  8. (Bonus - 2p)

    Ne propunem să implementăm o funcție de criptare numită encryptVigenere.
    Mai multe despre aceasta tehnică de criptare:
    http://practicalcryptography.com/ciphers/classical-era/vigenere-gronsfeld-and-autokey/
    
    Funcția encryptVigenere primește următoarele argumente: șirul de caractere alfabetice 
    care trebuie criptat (plaintext) și o cheie secreta (secret-key).
        
    Fie `xi`, indexul caracterului alfabetic de pe pozitia 'i' din sirul de caractere original.
    Fie `yi`, indexul caracterului alfabetic de pe pozitia 'i' din cheia secreta generată la pasul anterior.
    
    Caracterul criptat de pe pozitia 'i' va putea fi obținut alegând cel de-al `xi` caracter
    din alfabet, dacă alfabetul ar fi rotit cu `yi` poziții. (vom considera alfabetul indexat de la 0)
    
    Ex:
    plaintext: 'def'
    secretkey: 'bcd'
    
    Pentru prima poziție din șir: 
    `xi` = 'd' (caracterul cu indexul 3 din alfabet)
    `yi` = 'b' (caracterul cu indexul 1 din alfabet)
    Alfabetul rotit cu 1 pozitie: "bcdefghijklmnopqrstuvwxyza"
    Prin urmare, primul caracter criptat va fi: 'e' (al 3-lea element din alfabetul rotit)

    Daca cheia are o lungime mai mică decat șirul care trebuie criptat, 
    va fi duplicată pană va ajunge la aceeași dimensiune.
    
    > encryptVigenere "attackatdawn" "lemon"
    "lxfopvefrnhr"    
-}


test8 :: TestPP ()
test8 = tests 8 2
  [ testVal
    (encryptVigenere "attackatdawn" "lemon")
    "lxfopvefrnhr"
    "encryptVigenere 1" 1
  , testVal
    (encryptVigenere "theturtlemoves" "bcd")
    "ujhuwuunhnqyfu"
    "encryptVigenere 2" 1
  ]

encryptVigenere :: String -> String -> String
encryptVigenere plain secret = zipWith encryptChar plain $ cycle secret
    where
        base = ord 'a'
        encryptChar plain_ch key_ch = chr $ base + (ord plain_ch `mod` base + ord key_ch `mod` base) `mod` 26

--O solutie alternativa:
--encryptVigenere plain secret = zipWith tableToFunc rotTables plain
  --where
  --offsets = map (\c -> ord c - ord 'a') secret
  --rotTables = cycle $ map genRotTable offsets

{-
    9. (Bonus - 1p)     
    
    Ne propunem să implementăm o funcție de criptare numită oneTimePad.
    Mai multe despre aceasta tehnică de criptare:
    http://practicalcryptography.com/ciphers/classical-era/running-key/
    
    Funcția primește următoarele argumente: șirul de caractere care trebuie criptat (plain)
    și o valoare întreaga `seed`.
    
    1. Vom folosi parametrul `seed` pentru a genera o cheie secreta pseudo-aleatoare, de lungime egala
    cu lungimea șirului care trebuie criptat. 
    Hint: Apelați funcția randomAlphaKey
    
    2. In continuare, vom folosi aceeasi schema de criptare ca la exercitiul anterior.
    
    Observație: Implementați funcția „point-free” (doar primul parametru al
    funcției va fi explicit).
-}


test9 :: TestPP ()
test9 = testOne 5 $ testVal (oneTimePad "itsbettertolightacandlethancursethedarkness" 42) 
                                        "udeetfyssdinlvwvoyiscebmqrgbwixmmilrnsvfbhv" "oneTimePad" 1
        
oneTimePad :: String -> Int -> String
oneTimePad plain = encryptVigenere plain . randomAlphaKey

{-
Helpers for testing :)
-}


allTests = [test1, test2, test3, test4, test5, test6, test7, test8, test9]
check = runTestPP $ sequence_ allTests
[check1, check2, check3, check4, check5, check6, check7, check8, check9] = map runTestPP allTests

-}