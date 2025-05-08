-- Supondo que exista uma funcao vendas, que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas
-- na semana 0, vendas 1 devolve as vendas na semana 1, etc. Implemente uma funcao chamada vendaTotal, 
-- que recebe um argumento n e calcula todas as vendas da semana 0 ate a semana n. Observe que essa funcao deve ser recursiva. 
-- Exemplo de calculo: As vendas da semana 0 ate a semana 2, podem ser calculados usando a seguinte 
-- formula: vendas 0 + vendas 1 + vendas 2

vendas :: Int -> Int
vendas 0 = 100
vendas 1 = 200
vendas 2 = 300

vendaTotal :: Int -> Int
vendaTotal n
  | n == 0 = vendas 0
  | otherwise = vendas n + vendaTotal (n - 1)