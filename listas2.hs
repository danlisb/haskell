-- Função que retorna um booleando que diz se um inteiro está presente na lista
membro :: Int -> [Int] -> Bool
membro _ [] = False
membro x (y:ys)
    | x == y = True
    | otherwise = membro x ys

-- Função que conta o número de vezes que um inteiro aparece em uma lista
membroNum :: Int -> [Int] -> Int
membroNum _ [] = 0
membroNum x (y:ys)
    | x == y = 1 + membroNum x ys
    | otherwise = membroNum x ys

-- Defina a função membro usando a função membroNum
membro2 :: Int -> [Int] -> Bool
membro2 x xs = membroNum x xs > 0

-- Implemente uma função que retorna uma lista com os números que aparecem apenas uma vez na lista argumento. 
-- Ex: unico [2,4,1,4,1,3] -> [2,3]
-- A função membroNum deve ser usada na definição de unico.
unico :: [Int] -> [Int]
unico xs = filtraUnicos xs
  where
    filtraUnicos [] = []
    filtraUnicos (y:ys)
        | membroNum y xs == 1 = y : filtraUnicos ys
        | otherwise = filtraUnicos ys

-- função menores para quickSort
menores :: Int -> [Int] -> [Int]
menores _ [] = []
menores n (x:xs) = if x <= n then x : menores n xs else menores n xs

-- função maiores para quickSort
maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores n (x:xs) = if x > n then x : maiores n xs else maiores n xs

-- Quick Sort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort (menores x xs)
                   ++ [x] ++
                   quickSort (maiores x xs)