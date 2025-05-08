-- Escreva a funcao tresIguais que possui tipo que retorna True se seus tres argumentos sao iguais
tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

-- Escreva a funcao quatroIguais que possui tipo que retorna True se seus quatro argumentos sao iguais
quatroIguais :: Int -> Int -> Int -> Int -> Bool
quatroIguais x y z w = (x == y) && (y == z) && (z == w)

-- Defina a funcao que recebe 3 valores e diz quantos desses valores sao iguais
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z 
    |(x == y) && (y == z) = 3  
    |(x == y) || (x == z) || (y == z)= 2 
    |otherwise = 0 

--  Defina a funcao que retorna True se todos os seus argumentos sao diferentes. Obs: m /= n retorna True se m e n sao diferentes
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x /= y) && (y /= z) && (x /= z)

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

--  Escreva uma definicao de quantosSaoIguais que use a funcao todosDiferentes e a funcao todosIguais
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z 
    | todosIguais x y z = 3
    | todosDiferentes x y z = 0
    | otherwise = 2