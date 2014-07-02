{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Rotate a list N places to the left. N < 0 allowed.

rotate :: Int -> [x] -> [x]
rotate n xs
  | n == 0 = xs
  | n > 0 = let (head_xs, tail_xs) = splitAt n xs in tail_xs ++ head_xs
  | n < 0 = let (head_xs, tail_xs) = splitAt (length xs + n) xs in tail_xs ++ head_xs

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_empty n = rotate n ([]::[Char]) === [] 

prop_zero xs = rotate 0 xs === xs

prop_normal_pos x1s x2s = rotate (length x1s) (x1s ++ x2s) === (x2s ++ x1s)
prop_normal_neg x1s x2s = rotate (-(length x2s)) (x1s ++ x2s) === (x2s ++ x1s)

main = $(quickCheckAll)
