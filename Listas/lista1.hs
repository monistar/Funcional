-- Importações, funções que foram usadas, mas não estavam no repositório padrão da linguagem
import Data.List
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
-- Questão 18
reverso :: [Int] -> [Int]
reverso xs = reverse xs
-- Questão 19
divide :: Int -> [Int] -> ([Int], [Int])
divide n xs = (take n xs, drop n xs)
-- Questão 21
uniao :: [Int] -> [Int] -> [Int]
uniao xa xb = unique $ concat' xa xb
-- Questão 23
sequencia :: Int -> Int -> [Int]
sequencia a b = take a [b..] 
-- QUestão 24
inserir :: Int -> [Int] -> [Int]
inserir x xs = sort $ x:xs
