facA 0 = 0
facA 1 = 1
facA n = n* facA (n-1)


facB n = foldr (*) 1 [1 .. n ]
