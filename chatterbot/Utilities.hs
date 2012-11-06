module Utilities where

import Data.List

-- splitWithout: like splitAt, but discards the pivot element.
splitWithout :: Int -> [a] -> ([a], [a])
splitWithout i lst = (\(a,b) -> (a,tail b)) (splitAt i lst)

-- splitOn: split list on a given element, and discard that element.
--          Lambda function in foldl :: [[a]] -> [a] -> [[a]]
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn piv = foldr (\(y:_) xxs@(x:xs) -> if y == piv then []:xxs else (y:x):xs) 
                    [[]] . init . tails

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

