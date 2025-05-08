soma :: Int -> Int -> Int
soma x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

mult :: Int -> Int -> Int
mult x y = x * y

divInteira :: Int -> Int -> Int
divInteira x y = x `div` y

divFracao :: Double -> Double -> Double
divFracao x y = x / y

elevadoDois :: Int -> Int
elevadoDois x = x * x 

-- Defina a funcao elevadoQuatro :: Int -> Int que recebe um argumento n e devolve como resposta n^4 Use elevadoDois para definir elevadoQuatro
elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois (elevadoDois x)