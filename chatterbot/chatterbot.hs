-- Chatterbot program in Haskell
-- Johan Förberg F10 & Erik Henriksson π10                

import Data.List

-- joinWith: join lists with a given separator element.
joinWith :: [[a]] -> a -> [a]
joinWith []     _   = []
joinWith [x]    _   = x
joinWith (x:xs) sep = x ++ [sep] ++ (xs `joinWith` sep)

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

-- substitute: replace wildcards in string.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (a:as) b 
    | w==a      = b ++ (substitute w as b) 
    | otherwise = a :  (substitute w as b)

-- Test

s = "abc"
d = 'b'

