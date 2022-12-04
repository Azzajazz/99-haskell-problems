data Node a = One a | Many [Node a]

flatten' :: Node a -> [a]
flatten' (One x)   = [x]
flatten' (Many lst) = foldl (++) [] $ map flatten lst

-- More explicit
flatten :: Node a -> [a]
flatten (One x)         = [x]
flatten (Many [])       = []
flatten (Many (x : xs)) = flatten x ++ flatten (Many xs)