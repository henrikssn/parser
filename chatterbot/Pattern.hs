module Pattern where
import Utilities
import Data.List

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- substitute: replace wildcard elements in list.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc = (flip intercalate) . (splitOn wc)

-- match: According to problem description
-- TODO: Does only work with one wildcard and will return Nothing for more
--       than one.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match piv s@(a:as) t@(b:bs)
    | matches piv s t = matchHelper piv s t
    | otherwise = Nothing


matchHelper :: Eq a => a -> [a] -> [a] -> Maybe [a]
matchHelper piv s@(a:as) t@(b:bs)
    | a == b   = matchHelper piv as bs
    | a == piv = matchFirst piv [] as t
    | otherwise = Nothing

swm :: Eq a => [a] -> [a] -> Maybe [a]
swm (a:as) (b:bs)
    | as == bs = Just [b]
    | otherwise = Nothing
swm _ _ = Nothing

matchFirst :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
matchFirst _ _ [] _  = Nothing
matchFirst _ _ _  [] = Nothing
matchFirst piv m (a:as) (b:bs)
    | equals piv as bs = Just m
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
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
