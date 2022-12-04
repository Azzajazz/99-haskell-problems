import Prelude hiding (last)

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (_ : xs) = last xs