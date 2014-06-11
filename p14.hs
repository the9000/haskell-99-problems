{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad  -- for join

-- ## Duplicate the elements of a list.

dupli :: [x] -> [x]
dupli xs = reverse $ dupliTR xs []

dupliTR [] acc = acc
dupliTR (x:xs) acc = dupliTR xs (x:x:acc) 

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_main xs = dupli xs == join [[x, x] | x <- xs]

main = $(quickCheckAll)
