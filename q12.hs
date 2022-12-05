data Rle a = One a | Many (Int, a)

decode :: [Rle a] -> [a]
decode []                      = []
decode ((One el) : rest)       = el : decode rest
decode ((Many (n, el)) : rest) = replicate n el ++ decode rest