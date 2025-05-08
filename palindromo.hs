-- Implementar uma função que verifica se uma string contém um palindromo
palindromo :: String -> Bool
palindromo palavra = palavra == reverse palavra

palindromo2 :: String -> Bool
palindromo2 palavra
    | palavra == reverse palavra = True
    | otherwise = False