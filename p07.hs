{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Flatten nested structure

-- Nesting type
data NestedList a = Elem a | List [NestedList a]
                  deriving (Show, Eq)

-- Flatten
tr_flatten :: NestedList a -> [a] -> [a]
tr_flatten (Elem a) acc = (a:acc)
tr_flatten (List []) acc = acc
tr_flatten (List nest) acc = foldr (++) [] (map (\x -> tr_flatten x []) nest)

  
myFlatten x = tr_flatten x [] 

-- test

-- Failed to declare prop_trivial properly
-- prop_trivial :: [NestedList a] -> Property
-- prop_trivial xs = xs == [] ==> myFlatten (List xs) == []

prop_simple = myFlatten (List [Elem 1, Elem 3, Elem 3]) == [1, 2, 3]

prop_nested = myFlatten (List [Elem 1, List [Elem 2, Elem 3, List [Elem 4, Elem 5]]]) == [1, 2, 3, 4, 5]

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
