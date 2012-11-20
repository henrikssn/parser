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
                           where replaceWildcards el 
                                    | el == wc = sub
                                    | otherwise = [el]

-- match: try to match wildcards in a string and return the first match.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc aas@(a:as) bbs@(b:bs)
    | a == b  = match wc as bs
    | a == wc = orElse (swm wc aas bbs) (lwm wc aas bbs)
    | otherwise = Nothing

lwm :: Eq a => a -> [a] -> [a] -> Maybe [a]
lwm _ _ [] = Nothing
lwm wc aas@(_:as) (b:bs) = maybe (maybe Nothing (\x -> Just $ b:x) (lwm wc aas bs)) (\x -> Just [b]) (match wc as bs)

swm :: Eq a => a -> [a] -> [a] -> Maybe [a]
swm wc (_:as) (b:bs) = maybe Nothing (\x -> Just [b]) (match wc as bs) 

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
                                             (Just . substitute wc (snd pattern) . f)
                                             (match wc (fst pattern) str)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _   _ []     _   = Nothing
transformationsApply wc f ptns str = foldl transform Nothing ptns
                                     where transform res ptn
                                            | res == Nothing = transformationApply wc f str ptn
                                            | otherwise = res
