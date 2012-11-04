match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match w (a:as) (b:bs)
    | a == b = match w as bs
    | a == w = orElse (swm (a:as) (b:bs)) (lwm (a:as) (b:bs))
    | otherwise = Nothing

swm :: Eq a => [a] -> [a] -> Maybe [a]
swm (a:as) (b:bs)
    | equals as bs = Just [b]
    | otherwise = Nothing
swm _ _ = Nothing

lwm :: Eq a => [a] -> [a] -> Maybe [a]
lwm [] _ = Nothing
lwm _ [] = Nothing
lwm as bs
    | last as == last bs = lwm (init as) (init bs)
    | length as == 1 && length bs > 1 = Just bs
    | otherwise = Nothing

equals :: Eq a => [a] -> [a] -> Bool
equals [] [] = True
equals [] (_:_) = False
equals (_:_) [] = False
equals (a:as) (b:bs)
    | a == b = equals as bs
    | otherwise = False

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
