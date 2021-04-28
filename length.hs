

-- f :: (Ord t, Num t) => t -> [t] -> [t]
f :: (Ord t, Num t) => t -> [a] -> [a]
f _ [] = []
f y (x:xs) | y > 0  = f (y - 1) xs
           | otherwise = xs

list = f 3 [0..4]