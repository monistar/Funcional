-- Soma dois
somaDois :: Int -> Int -> Int
somaDois a b = a + b
-- Miolo da lista
miolo :: [Int] -> [Int]
miolo xs = init $ tail xs
-- Quantos iguais
iguais :: Int -> Int -> Int -> Int
iguais a b c = if a == b && b == c
                    then 3
                    else if a == b && b /= c || b == c && c /= a || a == c && c /= b
                        then 2
                        else 0
-- Maximo de 3
maximo :: Int -> Int -> Int -> Int
maximo a b c = max a $ max b c
-- Soma impares
somaImpares :: [Int] -> Int
somaImpares xs = sum [x| x <- xs, mod x 2 == 1]
-- Contar negativos
contarNegativos :: [Int] -> Int
contarNegativos xs = length [x | x <- xs, x < 0]
-- Pedaço do rabo
pedacoRabo :: Int -> [Int] -> [Int]
pedacoRabo n xs = reverse $ take n $ reverse xs
-- Gangorra mal feita
gangorra :: Int -> Int -> Int -> Int -> Int
gangorra a b c d = if a*b == c*d
                        then 0
                        else if a*b > c*d
                            then -1
                            else 1
-- Sublista
sublista :: Int -> Int -> [Int] -> [Int]
sublista de ate l = if ate < 0
                    then drop de $ reverse $ drop (abs ate) $ reverse l
                 else
                    drop de $ reverse $ drop (length l - ate) $ reverse l
                 