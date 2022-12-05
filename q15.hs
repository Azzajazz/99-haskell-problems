replicate' :: [a] -> Int -> [a]
replicate' [] _       = []
replicate' (x : xs) n = replicate n x ++ replicate' xs n