{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Find list length

myLength :: Integral n => [x] -> n 
myLength xs = myTRLength xs 0 

myTRLength :: Integral n => [x] -> n -> n
myTRLength [] acc = acc
myTRLength (x:xs) acc = myTRLength xs (acc + 1)

-- test

-- prop_zero xs = (xs == []) `trivial` myLength xs == 0

prop_normal xs = myLength xs == length xs

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
