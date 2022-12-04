at :: [a] -> Int -> Maybe a
at lst k
    | k == 1 = case lst of
        []    -> Nothing
        x : _ -> Just x
    | k > 1 = case lst of
        []     -> Nothing
        _ : xs -> at xs (k - 1)
    | otherwise = Nothing
