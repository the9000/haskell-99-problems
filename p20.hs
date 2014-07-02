{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Remove the K'th element from a list.

remove_at :: Int -> [x] -> [x]
remove_at n xs = reverse $ remove_at_tr n xs []

remove_at_tr :: Int -> [x] -> [x] -> [x]
remove_at_tr n [] acc = acc
remove_at_tr n (x:xs) acc
  | n /= 1 = remove_at_tr (n - 1) (xs) (x:acc)
  | n == 1 = remove_at_tr (n - 1) xs acc -- x is dropped

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_empty n = remove_at n ([]::[Char]) === [] 

prop_one xs = remove_at 1 xs === drop 1 xs

prop_normal_pos x1s y x2s = remove_at (length x1s + 1) (x1s ++ [y] ++ x2s) === (x1s ++ x2s)

main = $(quickCheckAll)
