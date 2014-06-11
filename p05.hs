{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## reverse a list

myRev :: [x] -> [x]
myRev xs = myTRRev xs []
  where
    myTRRev [] acc = acc
    myTRRev (x:xs) acc = myTRRev xs (x:acc) 

prop_normal xs = myRev xs == reverse xs

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
