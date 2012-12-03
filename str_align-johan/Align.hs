-- String alignment
-- By Johan Förberg F10, 911025-1817

module Align where

import Text.Printf   (printf)
import Control.Monad (forM_)

scMatch =     0
scMismatch = -1
scSpace =    -1
spc =        '-'

similarityScore :: String -> String  -> Int
similarityScore lst1 lst2 = sum . map gradeScore $ zip lst1 lst2

gradeScore :: (Char, Char) -> Int
gradeScore (x,   y) | x == spc  = scSpace
                    | y == spc  = scSpace
                    | x == y    = scMatch
                    | otherwise = scMismatch

attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 lst = [(h1:as, h2:bs) | (as, bs) <- lst]

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

type Alignment = (String, String)

optAlign :: String -> String -> (Int, [Alignment])
optAlign as bs = optLength (length as) (length bs)
  where optLength :: Int -> Int -> (Int, [Alignment])
        optLength i j = optTable !! i !! j

        optTable :: [[(Int, [Alignment])]]
        optTable  = [[optEntry i j | j <- [0..]]
                                   | i <- [0..]]

        optEntry :: Int -> Int -> (Int, [Alignment])
        optEntry 0 0 = (0, [("", "")])
        optEntry i 0 = (fst prev + gradeScore (a, spc), attachHeads a spc $ snd prev)
                where prev = optLength (i - 1) 0
                      a    = reverse as !! (i - 1)
        optEntry 0 j = (fst prev + gradeScore (spc, b), attachHeads spc b $ snd prev)
                where prev = optLength 0 (j - 1)
                      b    = reverse bs !! (j - 1)
        optEntry i j = foldr unwrap (0, []) $ maximaBy fst $ perms
                where -- Collect maxima in snd
                      unwrap (x, ys) (_, acc) = (x, ys ++ acc)
                      -- Possibly optimal alignments
                      perms = [(fst (optLength (i - 1) (j - 1)) + gradeScore (a,   b),
                                attachHeads a   b $ snd (optLength (i - 1) (j - 1)))
                              ,(fst (optLength (i)     (j - 1)) + gradeScore (spc, b),
                                attachHeads spc b $ snd (optLength (i)     (j - 1)))
                              ,(fst (optLength (i - 1) (j))     + gradeScore (a, spc),
                                attachHeads a spc $ snd (optLength (i - 1) (j)))
                              ]
                      -- Current character, counted from the end
                      a    = reverse as !! (i - 1)
                      b    = reverse bs !! (j - 1)

-- Brute force implementation
optAlignments :: String -> String -> [Alignment]
optAlignments (a:as) (b:bs) = maximaBy compare $ 
        (attachHeads a   b   $ optAlignments as     bs) ++
        (attachHeads spc b   $ optAlignments (a:as) bs) ++
        (attachHeads a   spc $ optAlignments as     (b:bs))
                where compare = uncurry similarityScore
optAlignments (a:as) ""     = attachHeads a   spc $ optAlignments as []
optAlignments ""     (b:bs) = attachHeads spc b   $ optAlignments [] bs
optAlignments ""     ""     = [("", "")]


outputAlignments :: String -> String -> IO ()
outputAlignments str1 str2 = do let align = snd $ optAlign str1 str2 
                                printf "There are %d alignments\n" $ length align
                                forM_ align $ \a -> 
                                    do putStrLn ""
                                       putStrLn $ fst a
                                       putStrLn $ snd a


