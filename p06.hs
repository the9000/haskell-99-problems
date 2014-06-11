{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Check if the list is a palindrome

isPaindrome :: [x] -> Bool
isPaindrome x = x == reverse x

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
