{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Insert an element at a given position into a list. 

insert_at :: Int -> x -> [x] -> [x]
insert_at n x xs
  | n > length xs  = error "Cannot insert past end"
  | n < 0 = error "Cannot insert before beginning"
  | otherwise = insert_at_tr n x xs []

insert_at_tr :: Int -> x -> [x] -> [x] -> [x]
insert_at_tr pos elt [] acc
  | pos == 0 = reverse (elt:acc)
  | otherwise = error ("Position " ++ (show pos) ++ " with empty target")
insert_at_tr pos elt (t:targets) acc
  | pos < 0 = error "Insert position below zero"
  | pos == 0 = (reverse acc) ++ (elt:t:targets)
  | otherwise = insert_at_tr (pos - 1) elt targets (t:acc)

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_first elt xs = insert_at 0 elt xs === (elt:xs)

prop_last x xs = insert_at (length xs) x xs === xs ++ [x]

prop_normal_pos x1s elt x2s = insert_at (length x1s) elt (x1s ++ x2s) === (x1s ++ [elt] ++ x2s)

main = $(quickCheckAll)

