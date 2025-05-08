-- Implementar uma função recursiva que recebe a base e o expoente e calcula a potência
-- > potencia 2 3 -> 8

potencia :: Int -> Int -> Int
potencia base expoente 
    | expoente == 0 = 1
    | expoente == 1 = base
    | otherwise = base * potencia base (expoente - 1)