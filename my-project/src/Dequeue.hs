{-|
Module      : Dequeue
This is a module that implements a dequeue and it's utilities.
-}
module Dequeue
 ( Dequeue (Dequeue)
 , emptyDEQ     -- :: Dequeue a
 , isEmptyDEQ   -- :: Dequeue a -> Bool
 , lengthDEQ    -- :: Dequeue a -> Int,
 , firstDEQ     -- :: Dequeue a -> Maybe a
 , lastDEQ      -- :: Dequeue a -> Maybe a
 , takeFrontDEQ -- :: Int -> Dequeue a -> [a]
 , takeBackDEQ  -- :: Int -> Dequeue a -> [a]
 , pushFrontDEQ -- :: Dequeue a -> a -> Dequeue a
 , popFrontDEQ  -- :: Dequeue a -> Maybe (a, Dequeue a)
 , pushBackDEQ  -- :: Dequeue a -> a -> Dequeue a
 , popBackDEQ   -- :: Dequeue a -> Maybe (a, q a)
 , fromListDEQ  -- :: [a] -> Dequeue a, O(n)
 ) where

-- |Function that returns an empty dequeue.
emptyDEQ :: Dequeue a
-- |Function that takes a dequeue and returns @True@ if it is empty or @Falese@ if it is not empty.
isEmptyDEQ :: Dequeue a -> Bool
-- |Function that takes a dequeue and returns it length.
lengthDEQ :: Dequeue a -> Int
-- |Function that takes a dequeue and returns the first element of it.
firstDEQ :: Dequeue a -> Maybe a
-- |Function that takes a dequeue and returns the last element of it.
lastDEQ  :: Dequeue a -> Maybe a
-- |Function that takes a number n and returns an array of first n elements in order they would be poped. If dequeue's length is smaller then n, it returns array made out of all elements of the dequeue.
takeFrontDEQ:: Int -> Dequeue a -> [a]
-- |Function that takes a number n and returns an array of last n elements in order they would be poped.  If dequeue's length is smaller then n, it returns array made out of all elements of the dequeue.
takeBackDEQ :: Int -> Dequeue a -> [a]
-- |Function that takes a dequeue and an element and adds it at the beginning of the dequeue.
pushFrontDEQ :: Dequeue a -> a -> Dequeue a
-- |Function that takes a dequeue and returns a @Just@ pair of the first element and the dequeue without the popped element, if dequeue is empty it returns @Nothing@.
popFrontDEQ :: Dequeue a -> Maybe (a, Dequeue a)
-- |Function that takes a dequeue and an element and adds it at the end of the dequeue.
pushBackDEQ  :: Dequeue a -> a -> Dequeue a
-- |Function that takes a dequeue and returns a @Just@ pair of the last element and the dequeue without the popped element, if dequeue is empty it returns @Nothing@.
popBackDEQ  :: Dequeue a -> Maybe (a, Dequeue a)
-- |Function that takes a list and returns a dequeue made out of that list.
fromListDEQ :: [a] -> Dequeue a

-- |Type that represents a dequeue.
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

takeFrontDEQ a (Dequeue as) = loop [] a as
 where loop acc 0 _ =acc
       loop acc _ []=acc
       loop acc n (x:xs)= loop ([x]++acc) (n-1) xs

takeBackDEQ a (Dequeue as) = loop [] a as
 where loop acc 0 _ =acc
       loop acc _ []=acc
       loop acc n x= loop ([(last x)]++acc) (n-1) (init x)

lastDEQ (Dequeue []) = Nothing
lastDEQ (Dequeue (a:[])) = Just a
lastDEQ (Dequeue (a:xs)) = lastDEQ (Dequeue xs)

pushFrontDEQ (Dequeue ax) a =Dequeue (a:ax)

popFrontDEQ (Dequeue []) = Nothing
popFrontDEQ (Dequeue (a:ax)) = Just(a,(Dequeue ax))

pushBackDEQ (Dequeue ax) a = Dequeue (ax++[a])

popBackDEQ (Dequeue [])= Nothing
popBackDEQ (Dequeue (a:[])) = Just (a, emptyDEQ)
--????????????

fromListDEQ []= emptyDEQ
fromListDEQ  (a:ax)= pushFrontDEQ (fromListDEQ ax) a
