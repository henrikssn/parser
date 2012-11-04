-- Chatterbot program in Haskell
-- Johan Förberg F10 & Erik Henriksson π10                

import Data.List

-- joinWith: join strings with a given separator character.
joinWith :: [String] -> Char -> String
joinWith []     _   = []
joinWith [x]    _   = x
joinWith (x:xs) sep = x ++ [sep] ++ (xs `joinWith` sep)

-- splitWithout: like splitAt, but discards the pivot element.
splitWithout :: String -> Int -> (String, String)
splitWithout ""  _ = ("", "")
splitWithout str i = (i `take` str, (i + 1) `drop` str)

-- splitOn: split string on a given character.
splitOn :: String -> Char -> [String]
splitOn ""  _   = ""
splitOn str piv = let pair = (maybe (str, "") 
                                    (splitWithout str)
                                    (piv `elemIndex` str))
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

