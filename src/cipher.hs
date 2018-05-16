import Data.Char
cipher :: [Char] -> Int -> [Char]

cipher [] n = []
cipher (x:xs) n = chr ((mod ( Data.Char.ord(x) - Data.Char.ord('a') + n )  26) +  Data.Char.ord('a')) : cipher xs n
