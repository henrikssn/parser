module Pattern where

import Utilities
import Data.List
import Data.Maybe

-------------------------------------------------------
-- Match and substitute
-------------------------------------------------------

-- substitute: replace wildcard elements in list.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc list sub = concat $ map replaceWildcards list
                           where replaceWildcards el = if el == wc then sub else [el]

-- match: try to match wildcards in a string and return the first match.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _   []       [] = Just []
match _   []       _  = Nothing
match _   _        [] = Nothing
match wc s@(a:as) t@(b:bs)
    | a == b                        = match wc as bs
    | a == wc && matches wc s t   = matchFirst wc [] s t
    | otherwise                     = Nothing

matchFirst :: Eq a => a -> [a] -> [a] -> [a] -> Maybe [a]
matchFirst _ _ [] _  = Nothing
matchFirst _ _ _  [] = Nothing
matchFirst wc m (a:as) (b:bs)
    | equals wc as bs = Just (m++[b])
    | otherwise = matchFirst wc (m++[b]) (a:as) bs


matches :: Eq a => a -> [a] -> [a] -> Bool
matches _ [] [] = True
matches _ [] _  = False
matches _ _  [] = False
matches wc (p:pattern) (x:xs)
    | p == x = matches wc pattern xs
    | p == wc =    if (equals wc pattern xs) then
                        matches wc pattern xs
                    else
                        matches wc (p:pattern) xs
    | otherwise = False


equals :: Eq a => a -> [a] -> [a] -> Bool
equals _ [] [] = True
equals _ [] _  = False
equals _ _  [] = False
equals wc (x:xs) (y:ys)
    | x == y = equals wc xs ys
    | x == wc = True
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
transformationApply wc f str pattern = maybe Nothing
                                             (Just . substitute wc (f . snd $ pattern)) 
                                             (match wc (fst pattern) str)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _   _ []     _   = Nothing
transformationsApply wc  f (x:xs) str = maybe (transformationsApply wc f xs str)
                                              Just
                                              (transformationApply wc f str x)
                                              
