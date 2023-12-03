module Main (main) where

import TestDay01
import TestDay02
import TestDay03

main :: IO ()
main = do
    TestDay01.test
    TestDay02.test
    TestDay03.test
