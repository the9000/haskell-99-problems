{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Split a list into two parts; the length of the first part is given.

splitN :: Int -> [x] -> ([x], [x])
splitN n xs = splitTR n xs []

splitTR :: Int -> [x] -> [x] -> ([x], [x])
splitTR 0 xs ys  = (reverse ys, xs)
splitTR n [] ys = (reverse ys, [])
splitTR n (x:xs) ys = splitTR (n - 1) xs (x:ys)

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_empty n = splitN n ([]::[Char]) === ([], [])

prop_zero xs = splitN 0 xs === ([], xs)

prop_normal x1s x2s = splitN (length x1s) (x1s ++ x2s) === (x1s, x2s)

main = $(quickCheckAll)
