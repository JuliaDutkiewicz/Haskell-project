{-|
Module      : Queue
This is a module that implements a queue and it's utilities.
-}
module Queue
  ( Queue (Queue)
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  , takeFrontQ -- :: Int -> Queue a -> [a]
  , fromListQ -- :: [a] -> Queue a
  )where

--interface
-- |Function that returns an empty queue.
emptyQ :: Queue a
-- |Function tht takes a queue and returns @True@ if it is emty or @False@ if it is not empty.
isEmptyQ :: Queue a -> Bool
-- |Function that takes an element and adds it to the queue.
addQ :: a -> Queue a -> Queue a
-- |Function that takes a queue a returns @Just@ pair of the first element and the queue without this element, If queue is empty it returns @Nothing@.
remQ :: Queue a -> Maybe ( a, Queue a)

-- |Function that takes a number n and returns an array of first n elements in order they would be removed. If queue's length is smaller then n, it returns array made out of all elements of the queue.
takeFrontQ:: Int -> Queue a -> [a]

-- |Function that takes a list and returns a queue made out of that list.
fromListQ :: [a] -> Queue a

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


takeFrontQ _ (Queue []) = []
takeFrontQ 0 _ = []
takeFrontQ a d = (takeFrontQ (a-1) (snd (extractMaybe (remQ d))))++ [fst (extractMaybe (remQ d))]

fromListQ []= emptyQ
fromListQ  a= addQ (last a) (fromListQ (init a))

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just a) = a
