-- Implementar a função slice, que devolve um pedaço interno de uma lista:
-- slice 3 7 [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’,’i’,’k’] -> "cdefg"
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i j xs
    | i < 1 || j < 1 || i > j = []
    | otherwise = slice' i j xs
    where
        slice' _ 0 _ = []
        slice' 1 j (x:xs) = x : slice' 1 (j - 1) xs
        slice' i j (_:xs) = slice' (i - 1) (j - 1) xs

-- Implementar a função compress que elimina elementos repetidos consecutivos
-- > compress "aaaabccaadeeee" "abcade"
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress' x xs
    where
        compress' _ [] = []
        compress' y (z:zs)
            | y == z = compress' y zs
            | otherwise = z : compress' z zs

-- Implementar a função pack que coloca elementos repetidos consecutivos em sublistas
-- > pack [’a’, ’a’, ’a’, ’a’, ’b’, ’c’, ’c’, ’a’, ’a’, ’d’, ’e’, ’e’, ’e’, ’e’] -> ["aaaa","b","cc","aa","d","eeee"]
-- > pack "bbaccczwwwq" -> ["bb","a","ccc","z","www","q"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' x xs
    where
        pack' y [] = [[y]]
        pack' y (z:zs)
            | y == z = (y : head rest) : tail rest
            | otherwise = [y] : pack' z zs
            where
                rest = pack' z zs

-- Implementar a função encode:
-- > encode "aaaabccaadeeee" -> [(4,’a’),(1,’b’),(2,’c’),(2,’a’),(1,’d’),(4,’e’)]
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = encode' x xs 1
    where
        encode' y [] count = [(count, y)]
        encode' y (z:zs) count
            | y == z = encode' y zs (count + 1)
            | otherwise = (count, y) : encode' z zs 1

-- Implementar a função rotate: rotate 3 "abcdefgh" ->"defghabc"
-- rotate (-2) [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’] "ghabcdef"
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
    | n < 0 = rotate' (length xs + n) xs
    | otherwise = rotate' n xs
    where
        rotate' 0 xs = xs
        rotate' _ [] = []
        rotate' n (x:xs) = rotate' (n - 1) (xs ++ [x])