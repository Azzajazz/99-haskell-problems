data Rle a = One a | Many (Int, a) deriving (Show)

encodeMod :: Eq a => [a] -> [Rle a]
encodeMod lst = let
    encodeMod' :: Eq a => [a] -> Maybe a -> Int -> [Rle a]
    encodeMod' [] Nothing _ = []
    encodeMod' [] (Just el) count
        | count == 1 = [One el]
        | otherwise  = [Many (count, el)]
    encodeMod' (x : xs) Nothing _ = encodeMod' xs (Just x) 1
    encodeMod' (x : xs) (Just el) count
        | x == el               = encodeMod' xs (Just x) (count + 1)
        | x /= el && count == 1 = One el : encodeMod' (x : xs) Nothing 0
        | otherwise             = Many (count, el) : encodeMod' (x : xs) Nothing 0

    in
        encodeMod' lst Nothing 0