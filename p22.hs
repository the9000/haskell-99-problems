{-# LANGUAGE TemplateHaskell #-}

import qualified Data.IntMap as I
import System.Random

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Extract a given number of randomly selected elements from a list. 

-- We use a list of (index_from, index_to) pairs to describe both sampling and reordering.
-- We extract elements by index_from and reorder them by index_to.

-- extractIndices: make a sub-list of a list picking values at given indices.
extractIndices :: [Int] -> [x] -> [x]
extractIndices [] _ = []
-- extractIndices indices xs = I.toList $ extractIndicesMap indices xs 0 I.empty

data OrdPair = OrdPair Int Int  -- fst is 'from', snd is 'to'.
             deriving (Show, Ord, Eq)
src :: OrdPair -> Int
src (OrdPair n _) = n

dst :: OrdPair -> Int
dst (OrdPair _ n) = n

-- Extract liat elements using a list of ascending unique in-range indices.
-- Returns a map of (index, element).
extractIndicesMap :: [OrdPair] -> [x] -> Int -> I.IntMap x -> I.IntMap x
extractIndicesMap [] _ _ acc = acc
extractIndicesMap indices [] _ _ = error ("No data; indices remain " ++ (show indices))
extractIndicesMap (i:indices) (x:xs) cnt acc
  | src i == cnt = extractIndicesMap indices xs (cnt + 1) (I.insert (dst i) x acc)
  | otherwise = extractIndicesMap (i:indices) xs (cnt + 1) acc

-- quickCheckAll generates test cases for all 'prop_*' properties


main = $(quickCheckAll)

