{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

import Control.Monad  -- for join

-- ## Decode run-length encoding

data RLEntry cnt payload = Repeat cnt payload | Single payload
                         deriving (Eq, Show)



unpack :: [RLEntry Int payload] -> [payload]
unpack input = join $ reverse $ unpackTR input []

unpackTR :: [RLEntry Int payload] -> [[payload]] -> [[payload]]
unpackTR [] acc = acc
unpackTR ((Single a):xs) acc = unpackTR xs ([a]:acc)
unpackTR ((Repeat cnt a):xs) acc = unpackTR xs ((rep a cnt):acc)

-- tests

prop_empty = unpack([]::[RLEntry Int Char]) === []

prop_one x =  unpack [Single x] === [x]

prop_double_head a  = unpack [Repeat 2 a, Single b, Single c] === [a, a, b, c]
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
      result = (g1 ++ g2 ++ g3)
  in unpack [repro n1 a1, repro n2 a2, repro n3 a3] === result

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
