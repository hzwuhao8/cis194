zipTogether :: [a] -> [b] -> [(a,b)]

zipTogether [] x = []
zipTogether x [] = []
zipTogether (x:xs) (y:ys) = (x,y) : zipTogether xs ys
