module Main where

import Lib
import Heap
import MathLib
import Stack


main :: IO ()
main = do
  someFunc
  print (sayYo "Haskellers")
  print (heapSort [1,345,2,325,24,4])
  print (add5 4)
  print $ isEmptyHH emptyHe
  print $ isEmptyHH (makeHeapFromArray ([]::[Int]))
  print $ isEmptyHH (makeHeapFromArray [1,23,12,34])
  print $ (topH (emptyHe) ::Maybe Int)
  print $ topH $ makeHeapFromArray [1.3,2.4,1.2]
