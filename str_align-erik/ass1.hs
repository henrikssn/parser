matchScore = 0
spaceScore = -1
mismatchScore = -1


similarityScore :: String -> String -> Int
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score (x, y),
                                         similarityScore xs (y:ys) + score ('-', x),
                                         similarityScore (x:xs) ys + score ('-', y)]
similarityScore (x:xs) "" = similarityScore xs [] + score ('-', x)
similarityScore "" (y:ys) = similarityScore [] ys + score ('-', y)
similarityScore "" "" = 0

score :: (Char, Char) -> Int
score (c1, c2)
 | c1 == c2 = matchScore
 | c1 == '-' || c2 == '-' = spaceScore
 | otherwise = mismatchScore


-- attachHeads: Takes a list of tuples of lists and inserts h1 and 2 first at those lists.
-- Example: attachHeads '1' '2' [("abc","def"),("ghi",jkl")] = [("1abc","2def"), ("1ghi", "2jkl")]
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy valueFcn (x:xs) = foldl cmp [x] xs
      where cmp (b:bs) a
             | (valueFcn a) > (valueFcn b) = [a]
             | (valueFcn a) == (valueFcn b) = a:(b:bs)
             | otherwise = (b:bs)

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments (x:xs) (y:ys) = maximaBy cmp $ (attachHeads x   y $ optAlignments xs ys) ++
                                             (attachHeads '-' y $ optAlignments (x:xs) ys) ++
                                             (attachHeads x '-' $ optAlignments xs (y:ys))
    where cmp = sum . (map score) . zipTuple
          zipTuple (a,b) = zip a b
optAlignments (x:xs) "" = attachHeads x '-' $ optAlignments xs []
optAlignments "" (y:ys) = attachHeads '-' y $ optAlignments []  ys
optAlignments "" "" = [("","")]


-- Test values
string1 = "writers"
string2 = "vintner"
testSim = similarityScore string1 string2
checkSim = testSim == -5
testAlign = optAlignments string1 string2
checkAlign = testAlign == []
