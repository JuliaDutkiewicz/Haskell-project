module Main where

import Heap
import Queue
import Dequeue


main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello " ++ name ++"!")
  putStrLn ("Here are some example commands:")
  putStrLn ("--heapSort [1,345,2,325,24,4]--")
  print (heapSort [1,345,2,325,24,4])
  putStrLn ("--isEmptyHH emptyHe--")
  print $ isEmptyHH emptyHe
  putStrLn ("--isEmptyHH (makeHeapFromArray ([]::[Int]))--")
  print $ isEmptyHH (makeHeapFromArray ([]::[Int]))
  putStrLn "--isEmptyHH (makeHeapFromArray [1,23,12,34])--"
  print $ isEmptyHH (makeHeapFromArray [1,23,12,34])
  putStrLn "--topH (emptyHe) ::Maybe Int--"
  print $ (topH (emptyHe) ::Maybe Int)
  putStrLn "--EmptyH :: Heap Char--"
  print (EmptyH :: Heap Char)
  putStrLn "--topH $ makeHeapFromArray [1.3,2.4,1.2]--"
  print $ topH $ makeHeapFromArray [1.3,2.4,1.2]
  putStrLn "--emptyQ :: Queue Char--"
  print (emptyQ :: Queue Char)
  putStrLn "--(Queue [1,2,3]) :: Queue Integer--"
  print ((Queue [1,2,3]) :: Queue Integer)
  putStrLn "--addQ 7 ((Queue [1,2]))--"
  print (addQ 7 ((Queue [1,2])))
  putStrLn "--emptyDEQ :: Dequeue Int--"
  print (emptyDEQ :: Dequeue Int)
  putStrLn "--(Queue [] :: Queue Int) == (Queue [] ::Queue Int)--"
  print ((Queue [] :: Queue Int) == (Queue [] ::Queue Int))
  print "Write your short list to sort it: "
  xs <- getLine
  print $ heapSort xs
  print "It was a small joke!"
  print "Write your short list to sort one more time number after number"
  print "First numeber: "
  no1 <- getLine
  print "Second numeber: "
  no2 <- getLine
  print "Third numeber: "
  no3 <- getLine
  let n1 = read no1 ::Integer
      n2 = read no2 :: Integer
      n3 = read no3 :: Integer
  print $ heapSort $ n1:n2:n3:[]
  print "So you see: IT'S WORKING!!!"
