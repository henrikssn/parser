module Pattern where
import Utilities
import Data.List
import Data.Maybe

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- substitute: replace wildcard elements in list.
-- TODO: Fix it! Keeping my old working version for now.
substitute :: Eq a => a -> [a] -> [a] -> [a]
--substitute wc = (flip intercalate) . (splitOn wc)
substitute _ [] _ = []
substitute w (a:as) b
    | w==a = b ++ (substitute w as b)
    | otherwise = a : (substitute w as b)


-- match: According to problem description
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match piv s@(a:as) t@(b:bs)
    | matches piv s t = matchHelper piv s t
    | otherwise = Nothing


matchHelper :: Eq a => a -> [a] -> [a] -> Maybe [a]
matchHelper _ [] [] = Just []
matchHelper _ [] _  = Nothing
matchHelper _ _  [] = Nothing
matchHelper piv s@(a:as) t@(b:bs)
    | a == b   = matchHelper piv as bs
    | a == piv = matchFirst piv [] s t
    | otherwise = Nothing

matchFirst :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
matchFirst _ _ [] _  = Nothing
matchFirst _ _ _  [] = Nothing
matchFirst piv m (a:as) (b:bs)
    | equals piv as bs = Just (m++[b])
    | otherwise = matchFirst piv (m++[b]) (a:as) bs


matches :: Eq a => a -> [a] -> [a] -> Bool
matches _ [] [] = True
matches _ [] _  = False
matches _ _  [] = False
matches piv (p:pattern) (x:xs)
    | p == x = matches piv pattern xs
    | p == piv =    if (equals piv pattern xs) then
                        matches piv pattern xs
                    else
                        matches piv (p:pattern) xs
    | otherwise = False


equals :: Eq a => a -> [a] -> [a] -> Bool
equals _ [] [] = True
equals _ [] _  = False
equals _ _  [] = False
equals piv (x:xs) (y:ys)
    | x == y = equals piv xs ys
    | x == piv = True
    | otherwise = False


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply _ _ _ _ = Nothing
transformationApply piv f str pattern = maybe Nothing
                                              (\x -> Just (substitute piv (snd pattern) x))
                                              (match piv (fst pattern) str)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
