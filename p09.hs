{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

-- ## Pack consecutive duplicates of list elements into sublists. 

compress :: Eq a => [a] -> [[a]]
compress xs = reverse $ compressTR xs []

compressTR :: Eq a => [a] -> [[a]] -> [[a]]
compressTR [] acc = acc -- only for completeness
compressTR (x:xs) [] = compressTR xs [[x]]
compressTR (x:xs) (a:acc)
  | x == head a = compressTR xs ((x:a):acc)
  | otherwise = compressTR xs ([x]:a:acc)

-- tests

prop_empty = compress ([]::[Char]) === []

prop_one x = compress [x] === [[x]]

prop_double_head a  = compress [a, a, b, c] === [[a, a], [b], [c]]
                      where b = a + 1
                            c = a + 2
                            
uniq [] = True
uniq (x:xs) = (not (elem x xs)) && uniq xs
rep a n = take n $ repeat a

prop_triple_v a1 n1 a2 n2 a3 n3 =
  (all (>0) [n1, n2, n3]) && (uniq [a1, a2, a3]) ==>
  let g1 = (rep a1 n1)
      g2 = (rep a2 n2)
      g3 = (rep a3 n3)
      result = compress (g1 ++ g2 ++ g3)
  in result === [g1, g2, g3]

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
