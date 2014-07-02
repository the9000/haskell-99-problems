{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

import Control.Monad  -- for join

-- ## Repplicate the elements of a list N times.

repli :: Int -> [x] -> [x]
repli n xs = join [rep n x | x <- xs]
  where rep n x = take n $ repeat x

-- quickCheckAll generates test cases for all 'prop_*' properties

prop_two xs = repli 2 xs == join [[x, x] | x <- xs]

main = $(quickCheckAll)
