module Align where

{- A 'space' is represented by Nothing. This preserves generality
 - over types which may not have an obvious 'space' value.
 -}

scMatch =     0
scMismatch = -1
scSpace =    -1

encode :: String -> [Maybe Char]
encode = map (\ c -> if c == ' ' then Nothing else Just c)

similarityScore :: Eq a => [Maybe a] -> [Maybe a] -> Int
similarityScore lst1 lst2 = sum . map gradeScore $ zip lst1 lst2

gradeScore :: Eq a => (Maybe a, Maybe a) -> Int
gradeScore (Just x, Just y) | x == y    = scMatch
                            | otherwise = scMismatch
gradeScore (_,      _)      = scSpace
                            

