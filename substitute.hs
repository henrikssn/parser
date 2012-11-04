substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (a:as) b 
    | w==a      = b ++ (substitute w as b) 
    | otherwise = a :  (substitute w as b)

