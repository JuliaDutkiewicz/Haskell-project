module Dequeue
 ( Dequeue (Dequeue)
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int, O(1)
 , firstDEQ     -- :: Dequeue a -> Maybe a,  O(1)
 , lastDEQ      -- :: Dequeue a -> Maybe a, O(1)
-- , takeFrontDEQ -- :: Int -> Dequeue a -> [a], O(n)
-- , takeBackDEQ  -- :: Int -> Dequeue a -> [a], O(n)
 , pushFrontDEQ -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popFrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a), O(1) amortised
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a, O(1) amortised
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, q a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 ) where

emptyDEQ :: Dequeue a
isEmptyDEQ :: Dequeue a -> Bool
lengthDEQ :: Dequeue a -> Int
firstDEQ :: Dequeue a -> Maybe a
lastDEQ  :: Dequeue a -> Maybe a
--takeFrontDEQ:: Int -> Dequeue a -> [a]
--takeBackDEQ :: Int -> Dequeue a -> [a]
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
pushBackDEQ  :: Dequeue a -> a -> Dequeue a
popBackDEQ  :: Dequeue a -> Maybe (a, Dequeue a)
fromListDEQ :: [a] -> Dequeue a

data Dequeue a = Dequeue [a] deriving Show

instance Eq a => Eq (Dequeue a) where
  (==) (Dequeue []) (Dequeue []) = True
  (==) (Dequeue []) _ = False
  (==) _ (Dequeue []) = False
  (==) (Dequeue a) (Dequeue b) = a==b

emptyDEQ = Dequeue []

isEmptyDEQ (Dequeue []) = True
isEmptyDEQ (Dequeue a) = False

lengthDEQ (Dequeue a) = length a

firstDEQ (Dequeue []) = Nothing
firstDEQ (Dequeue (a:xa)) =Just a

lastDEQ (Dequeue []) = Nothing
lastDEQ (Dequeue (a:[])) = Just a
lastDEQ (Dequeue (a:xs)) = lastDEQ (Dequeue xs)

pushFrontDEQ (Dequeue ax) a =Dequeue (a:ax)

popFrontDEQ (Dequeue []) = Nothing
popFrontDEQ (Dequeue (a:ax)) = Just(a,(Dequeue ax))

pushBackDEQ (Dequeue ax) a = Dequeue (ax++[a])

popBackDEQ (Dequeue [])= Nothing
popBackDEQ (Dequeue (a:[])) = Just (a, emptyDEQ)
popBackDEQ (Dequeue (a: xa)) = popBackDEQ(Dequeue xa)

fromListDEQ []= emptyDEQ
fromListDEQ  (a:ax)= pushFrontDEQ (fromListDEQ ax) a
