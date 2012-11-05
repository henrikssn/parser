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
match w s@(a:as) t@(b:bs)
    | a == b = match w as bs
    | a == w = orElse (swm s t) (lwm s t)
    | otherwise = Nothing



swm :: Eq a => [a] -> [a] -> Maybe [a]
swm (a:as) (b:bs)
    | as == bs = Just [b]
    | otherwise = Nothing
swm _ _ = Nothing

lwm (wc:ps) (x:xs) = Nothing
lwm [] _ = Nothing
lwm _ [] = Nothing
lwm as bs
    | last as == last bs = lwm (init as) (init bs)
    | length as == 1 && length bs > 1 = Just bs
    | otherwise = Nothing



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
