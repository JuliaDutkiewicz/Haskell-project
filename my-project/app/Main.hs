module Main where

import Heap
import Queue
import Dequeue


main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  print ("Hello " ++ name ++"!")
  print (heapSort [1,345,2,325,24,4])
  print $ isEmptyHH emptyHe
  print $ isEmptyHH (makeHeapFromArray ([]::[Int]))
  print $ isEmptyHH (makeHeapFromArray [1,23,12,34])
  print $ (topH (emptyHe) ::Maybe Int)
  print (EmptyH :: Heap Char)
  print $ topH $ makeHeapFromArray [1.3,2.4,1.2]
  print (emptyQ :: Queue Char)
  print ((Queue [1,2,3]) :: Queue Integer)
  print (addQ 7 ((Queue [1,2])))
  print (emptyDEQ :: Dequeue Int)
  print ((Queue [] :: Queue Int) == (Queue [] ::Queue Int))
