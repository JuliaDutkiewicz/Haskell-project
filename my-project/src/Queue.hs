{-|
Module      : Queue
This is a modulte that is implementing a queue and it's utilities.
-}
module Queue
  ( Queue (Queue)
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  )where

--interface
-- |Function that returns an empty queue.
emptyQ :: Queue a
-- |Function tht takes a queue and returns @True@ if it is emty or @False@ if it is not empty.
isEmptyQ :: Queue a -> Bool
-- |Function that takes an element and adds it to the queue.
addQ :: a -> Queue a -> Queue a
-- |Function that takes a queue a returns @Just@ pair of the first element and the queue without htis element, If queue is empty it returns @Nothing@.
remQ :: Queue a -> Maybe ( a, Queue a)

--implementation
-- |Type that represents a queue.
data Queue a = Queue [a] deriving Show

instance Eq a => Eq (Queue a) where
  (==) (Queue a) (Queue b) = a==b

emptyQ = Queue []

isEmptyQ (Queue []) = True
isEmptyQ (Queue a) = False

addQ a (Queue xs) = Queue (xs ++ [a])

remQ (Queue [])= Nothing
remQ (Queue (s:xs))= Just (s, Queue xs)
