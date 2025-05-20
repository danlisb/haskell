-- 3 : [4,5] == [3,4,5]
-- True : False : False : [] == [True, False, False]
-- (1,2) : (3,4) : [] == [(1,2), (3,4)]

-- (:) :: Int -> [Int] -> [Int]
-- (:) :: Bool -> [Bool] -> [Bool]
-- (:) :: Char -> String -> String

-- concat [1,2,3] ++ [4,5,6] -> [1,2,3,4,5,6]

-- A recursão sobre listas geralmente possui dois casos:
-- O caso da lista vazia [] 
-- O caso da lista não vazia. 

-- Função que soma os elementos de uma lista
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


-- Função que multiplica por dois os elementos de uma lista
multDois:: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2 * x : multDois xs

-- Função que recebe um inteiro e uma lista e multiplica todos os elementos da lista por esse inteiro
multLista :: Int -> [Int] -> [Int]
multLista _ [] = []
multLista n (x:xs) = n * x : multLista n xs

-- Função que recebe um inteiro e uma lista, e devolve um booleano dizendo se o inteiro se encontra na lista
elemento :: Int -> [Int] -> Bool
elemento _ [] = False
elemento n (x:xs) = n == x

-- Função que recebe um inteiro e uma lista, e diz quantas vezes o inteiro ocorre dentro da lista 
conta :: Int -> [Int] -> Int
conta _ [] = 0
conta n (x:xs) = if n == x then 1 + conta n xs else conta n xs

-- Função que recebe um inteiro e uma lista e conta quantos elementos da lista são maiores que o inteiro passado como argumento
contaMaiores :: Int -> [Int] -> Int
contaMaiores _ [] = 0
contaMaiores n (x:xs) = if x > n then 1 + contaMaiores n xs else contaMaiores n xs

-- Função que recebe um inteiro e uma lista e devolve uma lista contendo somente os valores que estavam na lista inicial
-- e que são maiores do que o inteiro passado como argumento
maiores :: Int -> [Int] -> [Int]
maiores _ [] = []
maiores n (x:xs) = if x > n then x : maiores n xs else maiores n xs

-- Função que recebe um inteiro m e um inteiro n e devolve uma lista contendo m vezes n 
-- geraLista 3 7 -> [7, 7, 7]
geraLista :: Int -> Int -> [Int]
geraLista 0 n = []
geraLista m n = n : geraLista (m - 1) n

-- Função que recebe um inteiro, uma lista e adiciona o elemento no fim da lista (sem usar o ++):
-- addFim 10 [1,2] -> [1,2,10]
addFim :: Int -> [Int] -> [Int]
addFim n [] = [n]
addFim n (x:xs) = x:addFim n xs

-- Função que recebe duas listas e concatena as mesmas gerando uma nova lista:
-- join [1,2,3] [4,5,6] -> [1,2,3,4,5,6]
join :: [Int] -> [Int] -> [Int]
join [] n = n
join (x:xs) n = x:join xs n

-- Função que recebe uma lista e devolve a mesma invertida
inverte :: [Int] -> [Int]
inverte [] = []
--inverte (x:xs) = inverte xs ++ [x]
inverte (x:xs) = join (inverte xs) [x]