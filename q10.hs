encode :: Eq a => [a] -> [(Int, a)]
encode lst = let
    encode' :: Eq a => [a] -> Maybe a -> Int -> [(Int, a)]
    encode' [] Nothing _ = []
    encode' [] (Just el) count = [(count, el)]
    encode' (x : xs) Nothing _ = encode' xs (Just x) 1
    encode' (x : xs) (Just el) count
        | x == el = encode' xs (Just x) (count + 1)
        | otherwise = (count, el) : encode' (x : xs) Nothing 0
    in
        encode' lst Nothing 0