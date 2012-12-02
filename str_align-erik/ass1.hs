-- Test scores
matchScore = 0
spaceScore = -1
mismatchScore = -1

-- similarityScore: Returns score of optimal alignment between
--                  two strings. Score is specified by 'score'
--                  function.
similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y,
                                         similarityScore xs (y:ys) + score '-' x,
                                         similarityScore (x:xs) ys + score '-' y]
similarityScore (x:xs) "" = similarityScore xs [] + score '-' x
similarityScore "" (y:ys) = similarityScore [] ys + score '-' y
similarityScore "" "" = 0

-- score: Returns score between two chars as specified by
--        'Test scores'.
score :: Char -> Char -> Int
score c1 c2
 | c1 == c2 = matchScore
 | c1 == '-' || c2 == '-' = spaceScore
 | otherwise = mismatchScore

-- simScore: Optimized version of similarityScore
simScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..]]
    simEntry :: Int -> Int -> Int
    simEntry 0 0 = 0
    simEntry i 0 = simLen (i-1) 0 + score '-' x
      where x = xs!!(i-1)
    simEntry 0 j = simLen 0 (j-1) + score '-' y
      where y = ys!!(j-1)
    simEntry i j = maximum [simLen (i-1) (j-1) + score x y,
                            simLen  i    (j-1) + score '-' y,
                            simLen (i-1)  j    + score x '-']
      where
        x = xs!!(i-1)
        y = ys!!(j-1)

-- attachHeads: Takes a list of tuples of lists and inserts h1 and 2 first at those lists.
-- Example: attachHeads '1' '2' [("abc","def"),("ghi",jkl")] = [("1abc","2def"), ("1ghi", "2jkl")]
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- maximaBy: Returns list of all maximum values as
--           specified by comparator function
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy valueFcn (x:xs) = foldl cmp [x] xs
      where cmp (b:bs) a
             | (valueFcn a) > (valueFcn b) = [a]
             | (valueFcn a) == (valueFcn b) = a:(b:bs)
             | otherwise = (b:bs)

type AlignmentType = (String,String)
-- optAlignments: Returns list of optimal alignments
--                between two strings
optAlignments :: String -> String -> [AlignmentType]
optAlignments (x:xs) (y:ys) = maximaBy compareScore $ (attachHeads x   y $ optAlignments xs ys) ++
                                                      (attachHeads '-' y $ optAlignments (x:xs) ys) ++
                                                      (attachHeads x '-' $ optAlignments xs (y:ys))
    where compareScore = sum . (map $ uncurry score) . uncurry zip
optAlignments (x:xs) "" = attachHeads x '-' $ optAlignments xs []
optAlignments "" (y:ys) = attachHeads '-' y $ optAlignments []  ys
optAlignments "" "" = [("","")]

-- optAlign: Optimized version of optAlignments
optAlign :: String -> String -> (Int, [AlignmentType])
optAlign xs ys = optLen (length xs) (length ys)
  where
    optLen i j = optTable!!i!!j
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..]]
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [("","")])
    optEntry 0 j = (fst item + score '-' y, attachHeads '-' y $ snd item)
      where
        item = optLen 0 (j-1)
        y = reverse ys!!(j-1)
    optEntry i 0 = (fst item + score x '-', attachHeads x '-' $ snd item)
      where
        item = optLen (i-1) 0
        x = reverse xs!!(i-1)
    optEntry i j = transform $ maximaBy fst $ [(fst (optLen (i-1) (j-1)) + score x y,attachHeads x y $  snd (optLen (i-1) (j-1))),
                                               (fst (optLen i (j-1)) + score '-' y, attachHeads '-' y $ snd (optLen i (j-1))),
                                               (fst (optLen (i-1) j) + score x '-', attachHeads x '-' $ snd (optLen (i-1) j))]
      where
        x = reverse xs!!(i-1)
        y = reverse ys!!(j-1)
        transform :: [(Int,[AlignmentType])] -> (Int, [AlignmentType])
        transform = foldr (\(x, ys) (_, acc) -> (x, ys ++ acc)) (0,[])

-- Test strings
string1 = "writers"
string2 = "vintner"
string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"
