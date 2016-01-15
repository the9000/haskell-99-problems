{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

lastbut1 [x, y] = x
lastbut1 (x:ys) = lastbut1 ys

prop_lastbut1 = lastbut1 [1, 2, 3] = 2 