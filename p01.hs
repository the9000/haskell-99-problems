{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

mylast :: [x] -> x
mylast [x] = x
mylast (x:xs) = mylast xs

prop_myLast xs = length xs > 0 ==> mylast xs == last xs

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
