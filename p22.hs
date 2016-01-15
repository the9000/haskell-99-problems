{-# LANGUAGE TemplateHaskell, CPP #-}

import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe
import System.Random

import Test.QuickCheck
import Test.QuickCheck.All

-- We don't want to really create and install modules for our makeshift code,
-- #include to the rescue.

#include "SparseRange.hs"


-- ## Extract a given number of randomly selected elements from a list.
-- To have unskewed results, we choose random indices in the list
-- by removing numbers from a sparsely represented sequence.
-- The source sequence is transformed into an IntMap to make index access cheaper.

-- randomIndices ::

pickByIndices :: [a] -> [Int] -> [a]
pickByIndices source indices = pickByIndices_ source_map indices
  where source_map = I.fromAscList $ zip [0..] source

pickByIndices_ :: I.IntMap a -> [Int] -> [a]
pickByIndices_ _ [] = []
pickByIndices_ source_map (n:ns) = ((I.!) source_map n) : pickByIndices_ source_map ns

randomIndices :: StdGen -> Int -> [Int]
randomIndices rgen size = randomIndices_ rgen size $ Subrange 0 (size - 1)

randomIndices_ :: StdGen -> Int -> SparseRange -> [Int]
randomIndices_ _ 0 _ = []
randomIndices_ rgen size range = next_index : (randomIndices_ next_rgen next_size next_range)
  where (next_index, next_range) = cutAt random_position range
        next_size = size - 1
        (random_value, next_rgen) = next rgen
        random_position = truncate (factor * fromIntegral random_value)
        factor = fromIntegral size / fromIntegral (gen_high - gen_low)
        (gen_low, gen_high) = genRange rgen

pickRandomItems :: StdGen -> [a] -> [a]
pickRandomItems rgen source = pickByIndices source $ randomIndices rgen $ length source

-- quickCheckAll generates test cases for all 'prop_*' properties

-- test pickByIndices

-- prop_EmptyIndicesMakeEmptyResult xs = pickByIndices [] xs === []

-- main = $(quickCheckAll)
