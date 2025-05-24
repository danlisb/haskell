-- Implementar a função pegaPosiao
-- >pegaPosicao 3 [10,2,11,4,5] -> 11
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao _ [] = 0
pegaPosicao n xs
    | n <= 0 || n > length xs = 0
    | otherwise = pegaPosicao' n xs
    where
        pegaPosicao' 1 (x:_) = x
        pegaPosicao' n (_:xs) = pegaPosicao' (n - 1) xs

-- Implementar a função pega: > pega 3 [1,2,3,4,5] -> [1,2,3]
pega :: Int -> [Int] -> [Int]
pega _ [] = []
pega n xs
    | n <= 0 = []
    | otherwise = pega' n xs
    where
        pega' 0 _ = []
        pega' _ [] = []
        pega' n (x:xs) = x : pega' (n - 1) xs

-- Implementar a função retira: > retira 3 [1,2,3,4,5] -> [4,5]
retira :: Int -> [Int] -> [Int]
retira _ [] = []
retira n xs
    | n <= 0 = xs
    | otherwise = retira' n xs
    where
        retira' 0 xs = xs
        retira' _ [] = []
        retira' n (_:xs) = retira' (n - 1) xs

-- Implementar a função mediaLista que aula a média dos elementos de uma lista
mediaLista :: [Int] -> Float
mediaLista [] = 0
mediaLista xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Implementar a função pegaMaiores > pegaMaiores 3 [10,2,3,4,5] -> [10,4,5]
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores _ [] = []
pegaMaiores n xs
    | n <= 0 = []
    | otherwise = pegaMaiores' n xs
    where
        pegaMaiores' _ [] = []
        pegaMaiores' n (x:xs)
            | x > n = x : pegaMaiores' n xs
            | otherwise = pegaMaiores' n xs

-- Implementar a função contaMaiores > contaMaiores 3 [10,2,3,4,5,16] -> 4
contaMaiores :: Int -> [Int] -> Int
contaMaiores _ [] = 0
contaMaiores n xs
    | n <= 0 = 0
    | otherwise = contaMaiores' n xs
    where
        contaMaiores' _ [] = 0
        contaMaiores' n (x:xs)
            | x > n = 1 + contaMaiores' n xs
            | otherwise = contaMaiores' n xs