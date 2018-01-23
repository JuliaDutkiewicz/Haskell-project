module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Dequeue

main :: IO ()
main = do
  defaultMain (testGroup "Our Dequeue Tests" [emptyDEQTest, isEmptyDEQTest1, isEmptyDEQTest2, lengthDEQTest1, lengthDEQTest2,lengthDEQTest3, firstDEQTest1, firstDEQTest2, firstDEQTest3, lastDEQTest1,lastDEQTest2,lastDEQTest3])

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


{-
--takeFrontDEQ:: Int -> Dequeue a -> [a]
--takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ  :: Dequeue a -> a -> Dequeue a
popBackDEQ  :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a
-}
