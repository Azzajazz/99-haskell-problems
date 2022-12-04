lastTwo :: [a] -> Maybe (a, a)
lastTwo [] = Nothing
lastTwo [x] = Nothing
lastTwo [x, y] = Just (x, y)
lastTwo (_ : xs) = lastTwo xs