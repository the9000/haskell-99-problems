{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

-- ## Drop every nth element of list

dropEvery :: Int -> [x] -> [x]
dropEvery n [] = []
dropEvery 1 xs = []
dropEvery n xs = reverse $ dropEveryTR n xs []

dropEveryTR :: Int -> [x] -> [x] -> [x]
dropEveryTR n xs acc = dropPiece n n xs acc -- co-recursive

dropPiece :: Int -> Int -> [x] -> [x] -> [x]
dropPiece _ _ [] acc = acc
dropPiece 1 n (x:xs) acc = dropEveryTR n xs acc
dropPiece np n (x:xs) acc = dropPiece (np - 1) n xs (x:acc)

-- quickCheckAll generates test cases for all 'prop_*' properties

-- prop_two xs = repli 2 xs == join [[x, x] | x <- xs]

main = $(quickCheckAll)
