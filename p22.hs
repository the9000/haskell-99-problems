{-# LANGUAGE TemplateHaskell #-}

import qualified Data.IntMap as I
import qualified Data.List as L
import System.Random

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Extract a given number of randomly selected elements from a list. 

-- We use a list of (index_from, index_to) pairs to describe both sampling and reordering.
-- We extract elements by index_from and reorder them by index_to.

-- extractIndices: make a sub-list of a list picking values at given indices.
-- The result is ordered by the order of elements in indices.
extractIndices :: [Int] -> [x] -> [x]
extractIndices [] _ = []
extractIndices indices xs = [snd item | item <- I.toList mapping]
  where sorted_pairs = L.sort $ zip indices [0..]
        mapping = extractIndicesMap sorted_pairs xs 0 I.empty
        -- assuming that I.toList is always sorted by key.

-- Extract liat elements using a list of ascending unique in-range indices.
-- Indices: (source_position, destination_position), must be sorted by source_position.
-- Returns a map of (index, element), keyed by destination_index values.
extractIndicesMap :: [(Int, Int)] -> [x] -> Int -> I.IntMap x -> I.IntMap x
extractIndicesMap [] _ _ acc = acc
extractIndicesMap indices [] _ _ = error ("No data; indices remain " ++ (show indices))
extractIndicesMap (i:indices) (x:xs) cnt acc
  | fst i == cnt = extractIndicesMap indices xs (cnt + 1) (I.insert (snd i) x acc)
  | otherwise = extractIndicesMap (i:indices) xs (cnt + 1) acc

-- quickCheckAll generates test cases for all 'prop_*' properties


main = $(quickCheckAll)

