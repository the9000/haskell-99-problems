{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Find the K'th element of a list. The first element in the list is number 1. 

kthElement :: Integral n => [x] -> n -> x
kthElement [] n  = error "Empty list"
kthElement (x:ys) 1 = x
kthElement (x:ys) n = kthElement ys (n - 1)


-- prop_empty = kthElement [] 1 == error "Empty list"
-- ^ Way complex to test here

prop_oneElt x = kthElement [x] 1 == x

prop_firstElt xs x  = x > 0 ==> kthElement (x:xs) 1 == x
prop_lastElt xs x  = x > 0 ==> kthElement (xs ++ [x]) (length xs + 1) == x

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
