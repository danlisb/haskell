-- Defina, usando guardas, a função sinal que recebe um inteiro como entrada e devolve: -1 se a entrada for um número negativo,
-- 1, caso seja positivo ou 0 caso a entrada seja o número zero
sinal :: Int -> Int
sinal x
    | x < 0 = -1 -- usar o parenteses na chamada da funcao com numeros negativos
    | x > 0 = 1
    | otherwise = 0