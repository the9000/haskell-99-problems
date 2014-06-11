{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Find list but one element of list

lbo :: [x] -> x
lbo [] = error "Empty list"
lbo [x] = error "One-element list"
lbo [x, y] = x
lbo (x:xs) = lbo xs  

prop_lbo_2 x y = lbo [x, y] == x

prop_lbo_n xs a b = lbo (xs ++ [a, b]) == a


-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
