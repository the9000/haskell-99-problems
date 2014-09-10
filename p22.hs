{-# LANGUAGE TemplateHaskell #-}

import qualified Data.IntMap.Lazy as I
import qualified Data.List as L
import Data.Maybe
import System.Random

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Extract a given number of randomly selected elements from a list.

-- To generate a list of random indices, we remove a random element from a list.
-- Tho make removals efficient, we use an IntMap of ranges instead of a list.

{-| pickFromRangeImpl n range count -> (element, range, count)
  * n is the ordinal number of the element to pick, 0 to range-1.
  * range is an IntMap where key is the lower bound of a subrange, value the upper bound.
  * count is the sum of lengths of subranges, saves us the traversal cost.
  * element is the value picked.
-}
pickFromRangeImpl :: Int -> I.IntMap Int -> Int -> (Int, I.IntMap Int, Int)
pickFromRangeImpl _ _ 0 = error "Cannot pick from empty range"
pickFromRangeImpl n range count = (n + bias, split_range, count - 1)
  where (bias, subrange_left, subrange_right) = findBiasElement n range
        split_range = splitRange (n + bias) range subrange_left subrange_right

{-| Finds subrange where n belongs, counting the bias from range.
  * n: we are looking for n-th element of range.
  * range: the sparse map of subranges.
  * returns (bias, subrange_left, subrange_right)
-}
findBiasElement :: Int -> I.IntMap Int -> (Int, Int, Int)
findBiasElement n range = (bias, subrange_left, subrange_right)
  where range_elt = I.lookupLE n range 
        bias = findBias range_elt range 0
        (subrange_left, subrange_right) = extractSubrangeBounds range_elt
        extractSubrangeBounds Nothing = error ("Subrange not found for " ++ (show n))
        extractSubrangeBounds (Just (l, r)) = (l, r)

{-| Finds the bias inside range relative to n.
  * n: index we're looking at;
  * range: the sparse map;
  * acc: accumulator for recursion.

  n-index    0  1 2     3 4 5 6          7  8  9  10  # no more
  range: [  (1 -> 3),  (5  -> 8),       (12  ->   15)]
  bias       1  1 1     2 2 2 2          5  5  5  5  undefined
-}
find2Bias :: Int -> I.Intmap -> Int -> Int
find2Bias n range acc = do
  scan_elt = I.lookupLE n range
  if scan_elt is Nothing -> return acc
  else l, r <- scan_elt; if n > r then acc += (r - n)


findBias :: Maybe (Int, Int) -> I.IntMap Int -> Int -> Int
findBias Nothing _ acc = acc
findBias (Just (l, r)) range acc = unpacker prev
  where prev = I.lookupLT l range
        unpacker Nothing = acc
        unpacker (Just (prev_l, prev_r)) = findBias prev range (acc + l - prev_l)
        

-- | splitRange value_to_remove range subrange_left subrange_right -> updated_range 
splitRange :: Int -> I.IntMap Int -> Int -> Int -> I.IntMap Int
splitRange value_to_remove range subrange_left subrange_right
  | value_to_remove > subrange_right || value_to_remove < subrange_left = error message
  | subrange_left == value_to_remove = I.delete value_to_remove $
                                       form_right_half $ range
  | otherwise = I.adjust (\_ -> value_to_remove - 1) subrange_left $
                form_right_half $ range
  where message = "Value_to_remove " ++ (show value_to_remove) ++ " out of range " ++
                  (show subrange_left) ++ ".." ++ (show subrange_right)
        form_right_half r = if value_to_remove < subrange_right
                            then I.insert (value_to_remove + 1) subrange_right r
                            else r

{- |
-- We use a list of (index_from, index_to) pairs to describe both sampling and reordering.
-- We extract elements by index_from and reorder them by index_to.
-}

{- | Make a sub-list of a list picking values at given indices.
     The result is ordered by the order of elements in indices.
-}
extractIndices :: [Int] -> [x] -> [x]
extractIndices [] _ = []
extractIndices indices xs = [snd item | item <- I.toList mapping]
  where sorted_pairs = L.sort $ zip indices [0..]
        mapping = extractIndicesMap sorted_pairs xs 0 I.empty
        -- assuming that I.toList is always sorted by key.

-- | Extract last elements using a list of ascending unique in-range indices.
-- | Indices: (source_position, destination_position), must be sorted by source_position.
-- | Returns a map of (index, element), keyed by destination_index values.
extractIndicesMap :: [(Int, Int)] -> [x] -> Int -> I.IntMap x -> I.IntMap x
extractIndicesMap [] _ _ acc = acc
extractIndicesMap indices [] _ _ = error ("No data; indices remain " ++ (show indices))
extractIndicesMap (i:indices) (x:xs) cnt acc
  | fst i == cnt = extractIndicesMap indices xs (cnt + 1) (I.insert (snd i) x acc)
  | otherwise = extractIndicesMap (i:indices) xs (cnt + 1) acc

-- quickCheckAll generates test cases for all 'prop_*' properties

-- test splitRange

-- prop_splitRange_first :: Int -> Int 
prop_splitRange_first start size =
  size > 1 ==>
  splitRange start (I.fromList [(start, end)]) start end  === I.fromList [(start+1, end)]
  where end = start + size - 1

prop_splitRange_last start size =
  size > 1 ==>
  splitRange end (I.fromList [(start, end)]) start end  === I.fromList [(start, end-1)]
  where end = start + size - 1

-- TODO: generate correct input, predicates fail too often.
prop_splitRange_middle start size pos =
  size > 1 && (pos > start) && (pos < start + size - 2) ==>
  splitRange pos (I.fromList [(start, end)]) start end  ===
  I.fromList [(start, pos-1), (pos+1, end)]
  where end = start + size - 1

-- test extractIndices

{- extractIndices works, but I can't figure a proper test.
prop_extractIndices xs = all (> 0) xs ==>
                         map ("abcdefghijk" !!) xs === extractIndices xs "abcdefghijk"
-}

main = $(quickCheckAll)

{- 718 250 8000 -}