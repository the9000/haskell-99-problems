{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Extract a slice from a list. 

slice ::  Int -> Int -> [x] -> [x]
slice n k xs = take k $ drop n xs

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_empty n k = slice n k ([]::[Char]) === []

prop_zero n xs = slice n 0 xs === []

prop_normal x1s x2s x3s = slice (length x1s) (length x2s) (x1s ++ x2s ++ x3s) === x2s

main = $(quickCheckAll)
