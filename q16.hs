import Prelude hiding (drop)

drop :: [a] -> Int -> [a]
drop lst k = let
    drop' :: [a] -> Int-> Int -> [a]
    drop' [] _ _ = []
    drop' (x : xs) m n 
        | m == n    = drop' xs 1 n
        | otherwise = x : drop' xs (m + 1) n
    in
        drop' lst 1 k