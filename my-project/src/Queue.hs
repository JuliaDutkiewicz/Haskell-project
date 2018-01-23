module Queue
  ( Queue (Queue)
  , emptyQ   -- :: Queue a
  , isEmptyQ -- :: Queue a -> Bool
  , addQ     -- :: a -> Queue a -> Queue a
  , remQ     -- :: Queue a -> (a, Queue a)
  )where

--interface

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> Maybe ( a, Queue a)

--implementation

data Queue a = Queue [a] deriving Show

instance Eq a => Eq (Queue a) where
  (==) (Queue []) (Queue []) = True
  (==) (Queue []) _ = False
  (==) _ (Queue []) = False
  (==) (Queue a) (Queue b) = a==b

emptyQ = Queue []

isEmptyQ (Queue []) = True
isEmptyQ (Queue a) = False

addQ a (Queue xs) = Queue (xs ++ [a])

remQ (Queue [])= Nothing
remQ (Queue (s:xs))= Just (s, Queue xs)
