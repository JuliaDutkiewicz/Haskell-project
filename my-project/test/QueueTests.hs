module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Queue

main :: IO ()
main = do
  defaultMain (testGroup "Our Queue Tests" [emptyQTest, isEmptyQTest1, isEmptyQTest2, addQTest1, addQTest2, addQTest3, remQTest1, remQTest2, takeFrontQTest1, takeFrontQTest2, takeFrontQTest3])

emptyQTest :: TestTree
emptyQTest = testCase "Testing emptyQ"
  (assertEqual "Should give empty queue!" (Queue [] :: Queue Int) (emptyQ :: Queue Int))

isEmptyQTest1 :: TestTree
isEmptyQTest1 = testCase "Testing isEmptyQ on empty queue"
  (assertEqual "Should give True!" (True) (isEmptyQ (Queue [])))

isEmptyQTest2 :: TestTree
isEmptyQTest2 = testCase "Testing isEmptyQ on not empty queue"
  (assertEqual "Should give False!" (False) (isEmptyQ (Queue ['a','c','f','d'])))

addQTest1 :: TestTree
addQTest1 = testCase "Testing addQ on empty queue"
  (assertEqual "Should give Queue [1]" (Queue [1]) (addQ 1 (Queue [])))

addQTest2 :: TestTree
addQTest2 = testCase "Testing addQ on not empty queue"
  (assertEqual "Should give Queue \"acfdz\"" (Queue "acfdz" ) (addQ 'z' (Queue ['a','c','f','d']) ) )

addQTest3 :: TestTree
addQTest3 = testCase "Testing addQ on not empty queue 2"
  (assertEqual "Should give Queue [2,1,46,3]" (Queue [2,1,46,3]) (addQ 3 (Queue [2,1,46])))

remQTest1 :: TestTree
remQTest1 = testCase "Testing remQ on empty queue"
  (assertEqual "Should give Nothing" (Nothing) (remQ (Queue [] :: Queue Char)))

remQTest2 :: TestTree
remQTest2 = testCase "Testing remQ on not empty queue"
  (assertEqual "Should give Just (1, Queue [2,3])" (Just (1, Queue [2,3])) (remQ (Queue [1,2,3])))

takeFrontQTest1 :: TestTree
takeFrontQTest1 = testCase "Testing takeFrontQ on empty"
  (assertEqual "Should give []" ([] :: [Int]) (takeFrontQ 5 (Queue [] :: Queue Int)))

takeFrontQTest2 :: TestTree
takeFrontQTest2 = testCase "Testing takeFrontQ on 3 and Queue [1,3,5,7,9]"
  (assertEqual "Should give [5,3,1]" [5,3,1] (takeFrontQ 3 $ Queue [1,3,5,7,9]))

takeFrontQTest3 :: TestTree
takeFrontQTest3 = testCase "Testing takeFrontQ on 6 and Queue [1,3,5,7,9]"
  (assertEqual "Should give [9,7,5,3,1]" [9,7,5,3,1] (takeFrontQ 6 $ Queue [1,3,5,7,9]))
