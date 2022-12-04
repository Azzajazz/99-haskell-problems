import Prelude hiding (length)

length :: [a] -> Int
length lst = let length_tail :: [a] -> Int -> Int
                 length_tail [] acc = acc
                 length_tail (_ : xs) acc = length_tail xs (acc + 1)
                 in length_tail lst 0
