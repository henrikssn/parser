-- Chatterbot program in Haskell
-- Johan Förberg F10 & Erik Henriksson π10                

import Data.List

-- joinWith: join lists with a given separator list.
joinWith :: [[a]] -> [a] -> [a]
joinWith []     _   = []
joinWith [x]    _   = x
joinWith (x:xs) sep = x ++ sep ++ (xs `joinWith` sep)

-- splitWithout: like splitAt, but discards the pivot element.
splitWithout :: [a] -> Int -> ([a], [a])
splitWithout []  _ = ([], [])
splitWithout lst i = (i `take` lst, (i + 1) `drop` lst)

-- splitOn: split list on a given element.
splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn []  _   = []
splitOn lst piv = let pair = (maybe (lst, []) 
                                    (splitWithout lst)
                                    (piv `elemIndex` lst))
                  in [fst pair] ++ splitOn (snd pair) piv

-- substitute: replace wildcard elements in list.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc wlist slist = (joinWith (wlist `splitOn` wc) slist)

-- match: According to problem description
-- TODO: Does only work with one wildcard and will return Nothing for more
--       than one.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match w (a:as) (b:bs)
    | a == b = match w as bs
    | a == w = orElse (swm (a:as) (b:bs)) (lwm (a:as) (b:bs))
    | otherwise = Nothing

-- swm: According to description of "singleWildcardMatch"
swm :: Eq a => [a] -> [a] -> Maybe [a]
swm (a:as) (b:bs)
    | equals as bs = Just [b]
    | otherwise = Nothing
swm _ _ = Nothing

-- lwm: According to description of "longerWildcardMatch"
lwm :: Eq a => [a] -> [a] -> Maybe [a]
lwm [] _ = Nothing
lwm _ [] = Nothing
lwm as bs
    | last as == last bs = lwm (init as) (init bs)
    | length as == 1 && length bs > 1 = Just bs
    | otherwise = Nothing

-- equals: true if lists is equivalent, otherwise false
equals :: Eq a => [a] -> [a] -> Bool
equals [] [] = True
equals (a:as) (b:bs)
    | a == b = equals as bs
equals _ _ = False

-- orElse: returns first parameter if that is not Nothing, notherwise second parameter
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Test

s = "abc"
d = 'b'

