{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

-- ##  Run-length encoding of a list.

data RLEntry cnt payload = RLEntry cnt payload
                         deriving (Eq)

-- XXX does not work.
-- instance Show (Show (cnt, payload)) => (RLEntry cnt payload) where
--   show (RLEntry cnt payload) = "<" ++ show payload ++ "*" ++ show cnt ++ ">"

compress :: Eq a => [a] -> [RLEntry Int a]
compress xs = reverse $ compressTR xs []

compressTR :: (Num t_cnt, Eq a) => [a] -> [RLEntry t_cnt a] -> [RLEntry t_cnt a]
compressTR [] acc = acc -- only for completeness
compressTR (x:xs) [] = compressTR xs [RLEntry 1 x]
compressTR (x:xs) (entry:acc)
  | x == chr = compressTR xs ((RLEntry (cnt + 1) chr):acc)
  | otherwise = compressTR xs ((RLEntry 1 x):entry:acc)
  where RLEntry cnt chr = entry

-- tests

prop_empty = compress ([]::[Char]) === []

prop_one x = compress [x] === [RLEntry 1 x]

prop_double_head a  = compress [a, a, b, c] === [RLEntry 2 a, RLEntry 1 b, RLEntry 1 c]
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
  in result === [RLEntry n1 a1, RLEntry n2 a2, RLEntry n3 a3]

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
