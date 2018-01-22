--{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- test/LibTests.hs
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Heap
import Heap (Heap(..))
import Heap ()

main :: IO ()
main = do
  defaultMain (testGroup "Our Heap Tests" [clearHTest, emptyHeTest, isEmptyHHTest1, isEmptyHHTest2, pushHTest, pushHTest2, popHTest, popHTest2, topHTest, topHTest2, heapSortTest ])

clearHTest :: TestTree
clearHTest = testCase "Testing clearH"
  (assertEqual "Should give empty heap!" (EmptyH) (clearH (Node 3 [])))

emptyHeTest :: TestTree
emptyHeTest = testCase "Testing emptyHe"
  (assertEqual "Should give empty heap" (EmptyH ::Heap Int) (emptyHe :: Heap Int))

isEmptyHHTest1 :: TestTree
isEmptyHHTest1 = testCase "Testing isEmptyHH when empty"
  (assertEqual "Should write TRUE" True (isEmptyHH emptyHe))

isEmptyHHTest2 :: TestTree
isEmptyHHTest2 = testCase "Testing isEmptyHH when not empty"
  (assertEqual "Should write FALSE" False (isEmptyHH (Node 'z' [])))

pushHTest :: TestTree
pushHTest = testCase  "Testing pushH on Empty"
  (assertEqual "Should write Node 3 []" (Node 3 []) (pushH 3 (EmptyH :: Heap Int)))

pushHTest2 :: TestTree
pushHTest2 = testCase "Testing pushH on Node 2 []"
  (assertEqual "Should write Node 2 [Node 3 []]" (Node 2 [Node 3 []]) (pushH 3 (Node 2 [])) )

popHTest :: TestTree
popHTest = testCase "Testing popH on Empty"
  (assertEqual "Should write (Nothing, EmptyH)" (((Nothing :: Maybe Char), (EmptyH :: Heap Char))) (popH (EmptyH :: Heap Char)))

popHTest2 :: TestTree
popHTest2 = testCase "Testing popH on Node 2 [Node 3 [], Node 5[Node 7 []]]"
  (assertEqual "Should write (Nothing, EmptyH)" ((Just 2, Node 3 [Node 5[Node 7[]]])) (popH (Node 2 [Node 3 [], Node 5[Node 7 []]])))

topHTest :: TestTree
topHTest = testCase "Testing topH on Empty"
  (assertEqual "Should write Nothing" (Nothing :: Maybe Char) (topH (EmptyH::Heap Char)))

topHTest2 :: TestTree
topHTest2 = testCase "Testing topH on Node 2 [Node 3 [], Node 5[Node 7 []]]"
  (assertEqual "Should write Nothing" (Just 2) (topH (Node 2 [Node 3 [], Node 5[Node 7 []]])))

heapSortTest :: TestTree
heapSortTest = testCase "Testing heapSort on [534,123,53,1,4,23,44]"
 (assertEqual "Should write [1,4,23,44,54,123,534]" ([1,4,23,44,53,123,534]) (heapSort [534,123,53,1,4,23,44]) )

-- add5Test :: TestTree
-- add5Test = testCase "Testing add5"
--   (assertEqual "Should add 5 to get 10" 10 (add5 5))
