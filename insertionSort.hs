ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x: ins a xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

-- Você consegue usar a função iSort para achar o maior e o menor número dentro de uma lista?
maiorEmenor :: [Int] -> (Int, Int)
maiorEmenor [] = (0, 0)
maiorEmenor xs = (last sortedList, head sortedList)
    where sortedList = iSort xs