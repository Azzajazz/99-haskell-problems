import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse lst = let
    reverse_tail :: [a] -> [a] -> [a]
    reverse_tail [] acc       = acc
    reverse_tail (x : xs) acc = reverse_tail xs (x : acc) in
        reverse_tail lst []
