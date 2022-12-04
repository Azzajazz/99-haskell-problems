compress :: Eq a => [a] -> [a]
compress lst = let
    compress_head :: Eq a => [a] -> Maybe a -> [a]
    compress_head [] _          = []
    compress_head (x : xs) Nothing = x : compress_head xs (Just x)
    compress_head (x : xs) (Just y)
        | x /= y    = x : compress_head xs (Just x)
        | otherwise = compress_head xs (Just x)
    in
        compress_head lst Nothing