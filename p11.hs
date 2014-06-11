{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

-- ##  Run-length encoding of a list, but mark duplicates and singles separately.
-- NOTE: also covers #13.

data RLEntry cnt payload = Repeat cnt payload | Single payload
                         deriving (Eq, Show)

-- XXX does not work.
-- instance Show (Show (cnt, payload)) => (RLEntry cnt payload) where
--   show (RLEntry cnt payload) = "<" ++ show payload ++ "*" ++ show cnt ++ ">"

compress :: Eq a => [a] -> [RLEntry Int a]
compress xs = reverse $ compressTR xs []

compressTR :: (Num t_cnt, Eq a) => [a] -> [RLEntry t_cnt a] -> [RLEntry t_cnt a]
compressTR [] acc = acc -- only for completeness
compressTR (x:xs) [] = compressTR xs [Single x]
compressTR (x:xs) ((Single a):acc)
  | x == a = compressTR xs ((Repeat 2 a):acc)
  | otherwise = compressTR xs ((Single x):(Single a):acc)
compressTR (x:xs) ((Repeat cnt a):acc)
  | x == a = compressTR xs ((Repeat (cnt + 1) a):acc)
  | otherwise = compressTR xs ((Single x):(Repeat cnt a):acc)

-- tests

prop_empty = compress ([]::[Char]) === []

prop_one x = compress [x] === [Single x]

prop_double_head a  = compress [a, a, b, c] === [Repeat 2 a, Single b, Single c]
                      where b = a + 1
                            c = a + 2
                            
uniq [] = True
uniq (x:xs) = (not (elem x xs)) && uniq xs

rep a n = take n $ repeat a

repro cnt payload
  | cnt == 1 = Single payload
  | otherwise = Repeat cnt payload

prop_triple_v a1 n1 a2 n2 a3 n3 =
  (all (>0) [n1, n2, n3]) && (uniq [a1, a2, a3]) ==>
  let g1 = (rep a1 n1)
      g2 = (rep a2 n2)
      g3 = (rep a3 n3)
      result = compress (g1 ++ g2 ++ g3)
  in result === [repro n1 a1, repro n2 a2, repro n3 a3]

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
