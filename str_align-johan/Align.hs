module Align where

scMatch =     0
scMismatch = -1
scSpace =    -1

similarityScore :: String -> String  -> Int
similarityScore lst1 lst2 = sum . map gradeScore $ zip lst1 lst2

gradeScore :: (Char, Char) -> Int
gradeScore (' ', _)   = scSpace
gradeScore (_,   ' ') = scSpace
gradeScore (x,   y)   | x == y    = scMatch
                      | otherwise = scMismatch

attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 lst = [(h1:xs, h2:xs) | (xs, ys) <- lst]

{- Explanation: the function prepends h1 and h2 to the fst and snd list of each
 - pair in lst, respectively.
 -}

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _    []     = []
maximaBy valf (x:xs) = foldr select [x] xs 
                           where select e acc@(a:_) 
                                   | (valf e) >  (valf a)    = [e]
                                   | (valf e) == (valf a)    = e : acc
                                   | otherwise               = acc

mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
          where
              mcsLen i j = mcsTable !! i !! j

              mcsTable   = [[mcsEntry i j | j <- [0..]] 
                                          | i <- [0..]]
       
              mcsEntry :: Int -> Int -> Int
              mcsEntry _ 0 = 0
              mcsEntry 0 _ = 0
              mcsEntry i j
                | x == y    = 1 + mcsLen (i-1) (j-1)
                | otherwise = max (mcsLen i (j-1)) (mcsLen (i-1) j)
                where
                    x = xs !! (i-1)
                    y = ys !! (j-1)

type Alignment = (String, String)

optAlignments :: String -> String -> [Alignment]
optAlignments _ _ = []

