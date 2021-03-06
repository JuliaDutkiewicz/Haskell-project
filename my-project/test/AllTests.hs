module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import Data.List
import Queue
import Dequeue
import Heap

main :: IO ()
main = do
  quickCheck (prop_HeapSort :: [Int] -> Bool)
  quickCheck (prop_takeFrontDEQ :: [Int] -> Bool)
  quickCheck (prop_takeBackDEQ :: [Int] -> Bool)
  quickCheck (prop_takeFrontQ :: [Int] -> Bool)
  defaultMain tests


--
-- tests
--

tests :: TestTree
tests = testGroup "Tests" [queueTests, dequeueTests, heapTests]

queueTests :: TestTree
queueTests = testGroup "Our Queue Tests" [emptyQTest, isEmptyQTest1, isEmptyQTest2, addQTest1, addQTest2, addQTest3, remQTest1, remQTest2, takeFrontQTest1, takeFrontQTest2, takeFrontQTest3]

dequeueTests :: TestTree
dequeueTests = testGroup "Our Dequeue Tests" [emptyDEQTest, isEmptyDEQTest1, isEmptyDEQTest2, lengthDEQTest1, lengthDEQTest2,lengthDEQTest3, firstDEQTest1, firstDEQTest2, firstDEQTest3, lastDEQTest1,lastDEQTest2,lastDEQTest3, pushFrontDEQTest1, pushFrontDEQTest2, pushBackDEQTest1,pushBackDEQTest2, popBackDEQTest1,popBackDEQTest2,popFrontDEQTest1,popFrontDEQTest2,fromListDEQTest1,fromListDEQTest2,takeFrontDEQTest1,takeFrontDEQTest2,takeFrontDEQTest3, takeBackDEQTest1,takeBackDEQTest2, takeBackDEQTest3]

heapTests :: TestTree
heapTests = testGroup "Our Heap Tests" [clearHTest, emptyHeTest, isEmptyHHTest1, isEmptyHHTest2, pushHTest, pushHTest2, popHTest, popHTest2, topHTest, topHTest2, heapSortTest, heapSortTest2 ]

--
-- queueTests
--

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

--
-- dequeueTests
--

emptyDEQTest :: TestTree
emptyDEQTest = testCase "Testing emptyDEQ"
  (assertEqual "Should give empty dequeue!" (Dequeue [] :: Dequeue Int) (emptyDEQ :: Dequeue Int))

isEmptyDEQTest1 :: TestTree
isEmptyDEQTest1 = testCase "Testing isEmptyDEQ on empty dequeue"
  (assertEqual "Should give True" (True) (isEmptyDEQ (Dequeue [])))

isEmptyDEQTest2 :: TestTree
isEmptyDEQTest2 = testCase "Testing isEmptyDEQ on not empty"
  (assertEqual "Should give False" (False) (isEmptyDEQ (Dequeue "I want holidays!")))

lengthDEQTest1 :: TestTree
lengthDEQTest1 = testCase "Testing lengthDEQ on empty"
  (assertEqual "Should give 0" 0 (lengthDEQ $ Dequeue []))

lengthDEQTest2 :: TestTree
lengthDEQTest2 = testCase "Testing lengthDEQ on Dequeue [1,2,4,5]"
  (assertEqual "Should give 4" 4 (lengthDEQ $ Dequeue [1,2,4,5]))

lengthDEQTest3 :: TestTree
lengthDEQTest3 = testCase "Testing lengthDEQ on Dequeue \"I want to pass FP!\""
  (assertEqual "Should give 18" 18 (lengthDEQ $ Dequeue "I want to pass FP!"))

firstDEQTest1 :: TestTree
firstDEQTest1 = testCase "Testing firstDEQ on empty"
  (assertEqual "Should give Nothing" (Nothing :: Maybe Int) (firstDEQ (Dequeue [] :: Dequeue Int)))

firstDEQTest2 :: TestTree
firstDEQTest2 = testCase "Testing firstDEQ on Dequeue [9,8,3,2,4]"
  (assertEqual "Should give Just 9" (Just 9) (firstDEQ $ Dequeue [9,8,3,2,4]))

firstDEQTest3 :: TestTree
firstDEQTest3 = testCase "Testing firstDEQ on Dequeue \"Adeste!\""
  (assertEqual "Should give Just 'A'!" (Just 'A') (firstDEQ $ Dequeue "Adeste!"))

lastDEQTest1 :: TestTree
lastDEQTest1 = testCase "Testing lastDEQ on empty"
  (assertEqual "Should give Nothing" (Nothing :: Maybe Char) (lastDEQ (Dequeue [] :: Dequeue Char)))

lastDEQTest2 :: TestTree
lastDEQTest2 = testCase "Testing lastDEQ on Dequeue [9,8,3,2,4]"
  (assertEqual "Should give Just 4" (Just 4) (lastDEQ $ Dequeue [9,8,3,2,4]))

lastDEQTest3 :: TestTree
lastDEQTest3 = testCase "Testing lastDEQ on Dequeue \"Adeste!\""
  (assertEqual "Should give Just '!'!" (Just '!') (lastDEQ $ Dequeue "Adeste!"))

pushFrontDEQTest1 :: TestTree
pushFrontDEQTest1 = testCase "Testing pushFrontDEQ on empty 1"
  (assertEqual "Should give Dequeue [1]" (Dequeue [1]) (pushFrontDEQ (Dequeue [] :: Dequeue Int) 1) )

pushFrontDEQTest2 :: TestTree
pushFrontDEQTest2 = testCase "Testing pushFrontDEQ on Dequeue [1,2,3] 8"
  (assertEqual "Should give Dequeue [8,1,2,3]" (Dequeue [8,1,2,3]) (pushFrontDEQ (Dequeue [1,2,3]) 8))

popFrontDEQTest1 :: TestTree
popFrontDEQTest1 = testCase "Testing popFrontDEQ on empty"
  (assertEqual "Should give Nothing" (Nothing :: Maybe (Int, Dequeue Int)) (popFrontDEQ (Dequeue [] :: Dequeue Int)))

popFrontDEQTest2 :: TestTree
popFrontDEQTest2 = testCase "Testing popFrontDEQ on Dequeue [4,7,34,2]"
  (assertEqual "Should give Maybe (4, Dequeue [7,34,2])" (Just (4, Dequeue [7,34,2])) (popFrontDEQ $ Dequeue [4,7,34,2]))

pushBackDEQTest1 :: TestTree
pushBackDEQTest1 = testCase "Testing pushBackDEQ on empty 1"
  (assertEqual "Should give Dequeue [1]" (Dequeue [1]) (pushBackDEQ (Dequeue [] :: Dequeue Int) 1) )

pushBackDEQTest2 :: TestTree
pushBackDEQTest2 = testCase "Testing pushBackDEQ on Dequeue [1,2,3] 8"
  (assertEqual "Should give Dequeue [1,2,3,8]" (Dequeue [1,2,3,8]) (pushBackDEQ (Dequeue [1,2,3]) 8))

popBackDEQTest1 :: TestTree
popBackDEQTest1 = testCase "Testing popBackDEQ on empty"
  (assertEqual "Should give Nothing" (Nothing :: Maybe (Int, Dequeue Int)) (popBackDEQ (Dequeue [] :: Dequeue Int)))

popBackDEQTest2 :: TestTree
popBackDEQTest2 = testCase "Testing popBackDEQ on Dequeue [4,7,34,2]"
  (assertEqual "Should give Maybe (2, Dequeue [4,7,34])" (Just (2, Dequeue [4,7,34])) (popBackDEQ $ Dequeue [4,7,34,2]))

fromListDEQTest1 :: TestTree
fromListDEQTest1 = testCase "Testing fromListDEQ on empty"
  (assertEqual "Should give Dequeue []" ((Dequeue []) :: Dequeue Char) (fromListDEQ ([] :: [Char])))

fromListDEQTest2 :: TestTree
fromListDEQTest2 = testCase "Testing fromListDEQ on [1,2,3]"
  (assertEqual "Should give Dequeue [1,2,3]" (Dequeue [1,2,3]) (fromListDEQ [1,2,3]))

takeFrontDEQTest1 :: TestTree
takeFrontDEQTest1 = testCase "Testing takeFrontDEQ on empty"
  (assertEqual "Should give []" ([] :: [Int]) (takeFrontDEQ 5 ((Dequeue []):: Dequeue Int)))

takeFrontDEQTest2 :: TestTree
takeFrontDEQTest2 = testCase "Testing takeFrontDEQ on 3 and Dequeue [2,4,6,8,0]"
  (assertEqual "Should give [6,4,2]" [6,4,2] (takeFrontDEQ 3 $ Dequeue [2,4,6,8,0]))

takeFrontDEQTest3 :: TestTree
takeFrontDEQTest3 = testCase "Testing takeFrontDEQ on 10 and Dequeue [2,4,6,8,0]"
  (assertEqual "Should give [0,8,6,4,2]" [0,8,6,4,2] (takeFrontDEQ 10 $ Dequeue [2,4,6,8,0] ))

takeBackDEQTest1 :: TestTree
takeBackDEQTest1 = testCase "Testing takeBackDEQ on empty"
  (assertEqual "Should give []" ([] :: [Int]) (takeBackDEQ 5 ((Dequeue []):: Dequeue Int)))

takeBackDEQTest2 :: TestTree
takeBackDEQTest2 = testCase "Testing takeBackDEQ on 3 and Dequeue [2,4,6,8,0]"
  (assertEqual "Should give [6,8,0]" [6,8,0] (takeBackDEQ 3 $ Dequeue [2,4,6,8,0]))

takeBackDEQTest3 :: TestTree
takeBackDEQTest3 = testCase "Testing takeBackDEQ on 10 and Dequeue [2,4,6,8,0]"
  (assertEqual "Should give [2,4,6,8,0]" [2,4,6,8,0] (takeBackDEQ 10 $ Dequeue [2,4,6,8,0] ))

--
-- heapTests
--

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
  (assertEqual "Should write (Nothing, EmptyH)" (Nothing :: Maybe (Char, Heap Char)) (popH (EmptyH :: Heap Char)))

popHTest2 :: TestTree
popHTest2 = testCase "Testing popH on Node 2 [Node 3 [], Node 5[Node 7 []]]"
  (assertEqual "Should write (Nothing, EmptyH)" (Just (2, Node 3 [Node 5[Node 7[]]])) (popH (Node 2 [Node 3 [], Node 5[Node 7 []]])))

topHTest :: TestTree
topHTest = testCase "Testing topH on Empty"
  (assertEqual "Should write Nothing" (Nothing :: Maybe Char) (topH (EmptyH::Heap Char)))

topHTest2 :: TestTree
topHTest2 = testCase "Testing topH on Node 2 [Node 3 [], Node 5[Node 7 []]]"
  (assertEqual "Should write Nothing" (Just 2) (topH (Node 2 [Node 3 [], Node 5[Node 7 []]])))

heapSortTest :: TestTree
heapSortTest = testCase "Testing heapSort on [534,123,53,1,4,23,44]"
 (assertEqual "Should write [1,4,23,44,54,123,534]" ([1,4,23,44,53,123,534]) (heapSort [534,123,53,1,4,23,44]) )

heapSortTest2 :: TestTree
heapSortTest2 = testCase "Testing heapSort on []"
  (assertEqual "Should write []" ([]::[Char]) (heapSort ([]::[Char])))

  --
  --quickCheck
  --

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
