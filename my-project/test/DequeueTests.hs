module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Dequeue

main :: IO ()
main = do
   defaultMain (testGroup "Our Dequeue Tests" [emptyDEQTest, isEmptyDEQTest1, isEmptyDEQTest2, lengthDEQTest1, lengthDEQTest2,lengthDEQTest3, firstDEQTest1, firstDEQTest2, firstDEQTest3, lastDEQTest1,lastDEQTest2,lastDEQTest3, pushFrontDEQTest1, pushFrontDEQTest2, pushBackDEQTest1,pushBackDEQTest2, popBackDEQTest1,popBackDEQTest2,popFrontDEQTest1,popFrontDEQTest2,fromListDEQTest1,fromListDEQTest2,takeFrontDEQTest1,takeFrontDEQTest2,takeFrontDEQTest3, takeBackDEQTest1,takeBackDEQTest2, takeBackDEQTest3])

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
