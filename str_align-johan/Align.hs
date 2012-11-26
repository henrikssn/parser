module Align where

{- A 'space' is represented by Nothing. This preserves generality over types
 - which may not have an obvious 'space' value.
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
                            
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 lst = [(h1:xs, h2:xs) | (xs, ys) <- lst]

{- Explanation: the function prepends h1 and h2 to the fst and snd list of each
 - tuple in lst, respectively.
 -}

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _    []     = []
maximaBy valf (x:xs) = foldr select [x] xs 
                           where select e acc@(a:_) 
                                   | (valf e) > (valf a)     = [e]
                                   | (valf e) == (valf a)    = e : acc
                                   | otherwise               = acc

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments _ _ = []
