module Main where

import Test.QuickCheck
import Heap
import Queue
import Dequeue
import Data.List

prop_HeapSort :: [Int] -> Bool
prop_HeapSort xs =
  heapSort xs == sort xs

prop_takeFrontDEQ :: [Int] -> Bool
prop_takeFrontDEQ xs =
  takeFrontDEQ (length xs) (fromListDEQ xs) == reverse xs

prop_takeBackDEQ :: [Int] -> Bool
prop_takeBackDEQ xs =
  takeBackDEQ (length xs) (fromListDEQ xs) ==xs

prop_takeFrontQ::[Int] -> Bool
prop_takeFrontQ xs =
  takeFrontQ (length xs) (fromListQ xs) == reverse xs

main :: IO ()
main = do
 quickCheck (prop_HeapSort :: [Int] -> Bool)
 quickCheck (prop_takeFrontDEQ :: [Int] -> Bool)
 quickCheck (prop_takeBackDEQ :: [Int] -> Bool)
 quickCheck (prop_takeFrontQ :: [Int] -> Bool)
