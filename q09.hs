pack :: Eq a => [a] -> [[a]]
pack lst = let
    pack_acc :: Eq a => [a] -> [a] -> [[a]]
    pack_acc [] acc = [acc]
    pack_acc (x : xs) [] = pack_acc xs [x]
    pack_acc (x : xs) (y : ys)
        | x == y    = pack_acc xs (x : y : ys)
        | otherwise = (y : ys) : pack_acc (x : xs) []
    in
        pack_acc lst []