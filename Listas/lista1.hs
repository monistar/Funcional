-- Importações, funções que foram usadas, mas não estavam no repositório padrão da linguagem
import Data.List
import Data.String
-- Questão 01
menorDeDois :: Int -> Int -> Int
menorDeDois a b = min a b
-- Questão 02
menorDeTres :: Int -> Int -> Int -> Int
menorDeTres a b c = min a $ min b c
-- Questão 03
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
-- Questão 04
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-- Questão 05
nEsimo :: Int -> [Int] -> Int
nEsimo n xs = xs !! n
-- Questão 06
pertence :: Int -> [Int] -> Bool
pertence n xs = n `elem` xs
-- Questão 07
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho xs = (tamanho $ drop 1 xs) + 1
-- Questão 08
maior :: [Int] -> Int
maior [] = -1
maior xs | length xs == 1 = head xs
maior xs = if (head xs) > (maior $ drop 1 xs) 
            then head xs
           else maior $ drop 1 xs
-- Questão 09
frequencia :: Int -> [Int] -> Int
frequencia n [] = 0
frequencia n xs = if n == head xs
                    then (frequencia n $ drop 1 xs) + 1
                    else frequencia n $ drop 1 xs
-- Questão 10
unico :: Int -> [Int] -> Bool
unico n xs = if (frequencia n xs) == 1
                then True
                else False
-- Questão 11
maioresQue :: Int -> [Int] -> [Int]
maioresQue n xs = [x | x <- xs, x > n]
-- Questão 12
concat' :: [Int] -> [Int] -> [Int]
concat' xa xb = xa ++ xb
-- Questão 13
calda :: [Int] -> [Int]
calda xs = drop 1 xs
-- Questão 14
corpo :: [Int] -> [Int]
corpo xs = reverse $ drop 1 $ reverse xs
-- Questão 15
unique :: [Int] -> [Int]
unique [] = []
unique xs = if (head xs) `elem` (unique $ drop 1 xs)
                then unique $ drop 1 xs
                else head xs : (unique $ drop 1 xs)
-- Questão 16
menores :: Int -> [Int] -> [Int]
menores 0 xs = []
menores n xs = if (head xs) `elem` (take n $ sort xs)
                        then (head xs):menores (n-1) (tail xs)
                        else menores n (tail xs)
-- Questão 17
alter :: Int -> [Int]
alter 1 = [1, -1]
alter n = alter (n - 1) ++ [n, -n]
-- Questão 18
reverso :: [Int] -> [Int]
reverso xs = reverse xs
-- Questão 19
divide :: Int -> [Int] -> ([Int], [Int])
divide n xs = (take n xs, drop n xs)
-- Questão 20
intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala xa [] = xa
intercala [] xb = xb
intercala xa xb = (head xa):(head xb):intercala (tail xa) (tail xb)
-- Questão 21
uniao :: [Int] -> [Int] -> [Int]
uniao xa xb = unique $ concat' xa xb
-- Questão 22
intersec :: [Int] -> [Int] -> [Int]
intersec [] xb = []
intersec xa xb = if head xa `elem` xb
                        then (head xa):intersec (tail xa) xb
                        else intersec (tail xa) xb
-- Questão 23
sequencia :: Int -> Int -> [Int]
sequencia a b = take a [b..] 
-- Questão 24
inserir :: Int -> [Int] -> [Int]
inserir x xs = sort $ x:xs
-- Questão 25
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted xs | length xs == 1 = True
isSorted xs = if (head xs) <= (head $ drop 1 xs)
                then isSorted (drop 1 xs)
                else False
-- Questão 26
qSort :: [Int] -> [Int]
qSort xs = sort xs
-- Questão 27
rotEsq :: Int -> String -> String
rotEsq 0 xs = xs
rotEsq n xs = rotEsq (n - 1) (tail $ xs ++ [head xs])
-- Questão 28
rotDir :: Int -> String -> String
rotDir 0 xs = xs
rotDir n xs = rotDir (n - 1) (init $ (last xs):xs)
-- Questão 29
element :: Char -> [Char] -> Int
element c [] = -1
element c xc = if c == (head xc)
                then 26 - (length xc)
                else element c (tail xc)
upperString :: String -> String
upperString "" = ""
upperString s = if element (head s) ['a'..'z'] >= 0
                then (['A'..'Z']!!(element (head s) ['a'..'z'])):upperString (tail s)
                else (head s):upperString (tail s)
-- Questão 30
titulo :: String -> Bool -> String
titulo "" b = ""
titulo s b = if element (head s) ['a'..'z'] >= 0 && b == False
                then (head s):titulo (tail s) False
                else if element (head s) ['A'..'Z'] >= 0 && b == False
                        then (['a'..'z']!!(element (head s) ['A'..'Z'])):titulo (tail s) False
                        else if element (head s) ['a'..'z'] >= 0 && b == True
                                then (['A'..'Z']!!(element (head s) ['a'..'z'])):titulo (tail s) False
                                else if element (head s) ['A'..'Z'] >= 0 && b == True
                                        then (head s):titulo (tail s) False
                                        else (head s):titulo (tail s) True
frase :: String -> String
frase xs = titulo xs True
-- Questão 31
select :: String -> [Int] -> String
select s [] = []
select s xs = s !! (head xs) : (select s (tail xs))
-- Questão 32
isPalind :: String -> Bool
isPalind s = if s == reverse s
                then True
                else False
-- Questão 33
primos :: Int -> [Bool]
primos 0 = []
primos n = True : primos (n - 1) 
multiplos :: Int -> Int -> [Int]
multiplos n x = if n*x > 100
                    then []
                    else  [n*x] ++ (multiplos n (x+1))
inicio :: [Int] -> [Bool] -> [Bool]
inicio xn xp = take (head xn) xp
fim :: [Int] -> [Bool] -> [Bool]
fim xn xp = drop (head xn) xp
unir :: [Bool] -> [Bool] -> [Bool]
unir xs x = xs ++ x
nPrimos :: [Int] -> [Bool] -> [Bool]
nPrimos [] xp = xp
nPrimos xn xp = nPrimos (tail xn) ((unir (inicio xn xp)  [False]) ++ (fim xn xp))
crivo :: Int -> [Bool]-> [Bool]
crivo 2 xp = nPrimos (multiplos 2 2) xp
crivo n xp = crivo (n-1) (nPrimos (multiplos n 2) xp)
primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo n = (crivo n (primos 10)) !! n 
-- Questão 34
sdig :: Int -> Int
sdig 0 = 0
sdig n = sdig (div n 10) + (mod n 10)
-- Questão 35
sublista :: Int -> Int -> [Int] -> [Int]
sublista de ate l = if ate < 0
                    then drop de $ reverse $ drop (abs ate) $ reverse l
                 else
                    drop de $ reverse $ drop (length l - ate) $ reverse l
swap :: Int -> Int -> [Int] -> [Int]
swap x y xs = take (x - 1) xs ++ [xs !! (y-1)] ++ sublista x (y-1) xs ++ [xs !! (x-1)] ++ drop y xs
ordenar :: Int -> Int -> [Int] -> [Int]
ordenar i y xs = if i == (length xs) + 1
                        then xs
                        else if y == (length xs) + 1
                                then ordenar (i + 1) (i+2) xs
                                else if (xs !! (i-1)) > (xs !! (y-1))
                                        then ordenar i (y + 1) (swap i y xs)
                                        else ordenar i (y + 1) xs
bubbleSort :: [Int] -> [Int]
bubbleSort xs = ordenar 1 2 xs
-- Questão 36
aparicoes :: Int -> [Int] -> Int
aparicoes x xs = if (length xs) > 0 
                    then if x /= (head xs)
                            then 0
                            else (aparicoes x (tail xs)) + 1
                    else 0
junta :: Int -> Int -> [Int]
junta x n = if x == 1
                then [n]
                else [x, n]
compac :: [Int] -> [[Int]]
compac xs = if length xs > 0
                then if junta (aparicoes (head xs) xs) (head xs) == []
                        then compac (drop (aparicoes (head xs) xs) xs)
                        else junta (aparicoes (head xs) xs) (head xs):compac (drop (aparicoes (head xs) xs) xs)
                else []
-- Questão 37
impares :: [Int] -> [Int]
impares [] = []
impares xs = if mod (head xs) 2 == 1
                then (head xs): impares (tail xs)
                else impares (tail xs) 
pares :: [Int] -> [Int]
pares [] = []
pares xs = if mod (head xs) 2 == 0
                then (head xs): pares (tail xs)
                else pares (tail xs)
splitints :: [Int] -> ([Int],[Int])
splitints xs = (impares xs, pares xs)
-- Questão 38
perfeito :: Int -> Bool
perfeito x = x `elem` [p*p | p <- [1..1000]]
-- Questão 39
bas :: [Char]
bas = ['0'..'9']++['A'..'Z']
converte :: Int -> Int -> String
converte 0 b = ""
converte n b = converte (div n b) b ++ [bas !! (mod n b)]
base :: Int -> Int -> String
base n b = converte n b
-- Questão 40
tirarInicio :: [Int] -> Int -> [Int]
tirarInicio xs x = take (x-1) xs
tirarFim :: [Int] -> Int -> [Int]
tirarFim xs x = drop x xs
partes :: [Int] -> Int -> [[Int]]
partes [] _ = [[]]
partes xs x = if x <= (length xs)
                then xs:partes xs (x+1) ++ partes ((tirarInicio xs x)  ++ (tirarFim xs x)) x
                else [xs]
aparicoes' :: [Int] -> [[Int]] -> Int
aparicoes' _ [] = 0
aparicoes' x xs = if sort x == sort (head xs)
                        then aparicoes' x (tail xs) + 1
                        else aparicoes' x (tail xs)
repetidos :: [[Int]] -> [[Int]]
repetidos [] = []
repetidos xs = if (aparicoes' (head xs) xs) > 1
                        then repetidos (tail xs)
                        else repetidos (tail xs) ++ [head xs] 
produtoCartesiano :: [Int] -> [[Int]]
produtoCartesiano xs = sort $ repetidos (partes xs 1)
