module Pattern where

import Utilities
import Data.List
import Data.Maybe

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- substitute: replace wildcard elements in list.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc list sub = 
  concat $ map replaceWildcards list
    where replaceWildcards el = if el == wc then sub else [el]

-- match: try to match wildcards in a string and return the first match.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _   []       [] = Just []
match _   []       _  = Nothing
match _   _        [] = Nothing
match piv s@(a:as) t@(b:bs)
    | a == b                        = match piv as bs
    | a == piv && matches piv s t   = matchFirst piv [] s t
    | otherwise                     = Nothing

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
transformationApply piv f str pattern = maybe Nothing
                                              (\x -> Just (substitute piv (snd pattern) x))
                                              (match piv (fst pattern) str)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _   _ []     _   = Nothing
transformationsApply piv f (x:xs) str = maybe (transformationsApply piv f xs str)
                                              (\x -> Just x)
                                              (transformationApply piv f str x)
                                              