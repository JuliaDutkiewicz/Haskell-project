{-|
Module      : Heap
This is a module that implements a pairing heap and it's utilities.
-}
module Heap
--
  ( Heap(EmptyH,Node)
  , clearH
  , emptyHe
  , isEmptyHH
  , pushH
  , popH
  , topH
  , heapSort
  , makeHeapFromArray
  , makeArrayFromHeap
  ) where

--interfaces
-- |Fuction that takes a heap and return empty heap.
clearH    :: Heap a->Heap a
-- |Function that returns an empty heap.
emptyHe    :: Heap a
-- |Function that takes a dequeue and returns @True@ if it is empty or @Falese@ if it is not empty.
isEmptyHH  :: Heap a -> Bool
-- |Function that takes an element and a heap and returns a heap with this element added.
pushH     :: (Ord a)=>a -> Heap a -> Heap a
-- |Function that takes a heap and returns a @Just@ pair of the first element and the heap without the element, if heap is empty it returns @Nothing@.
popH      :: (Ord a)=>Heap a -> Maybe (a, Heap a)
-- |Function that takes a reap and returns @Just@ the element that is on the top of the heap (the smallest element).
topH      :: Heap a -> Maybe a
-- |Function that takes an array and returns a soted array made out of elements of taken array. It uses heap to sort the array.
heapSort  :: (Ord a)=>[a]->[a]
-- |Function that takes an array and returns heap made out of the elements of taken array.
makeHeapFromArray :: (Ord a)=> [a]-> Heap a
-- |Function that takes a heap and returns a array made out of the heap. It is sorted increasingly.
makeArrayFromHeap :: (Ord a)=> Heap a -> [a]

--implementation
-- | Type that represents a Heap, it can be Empty, or it can have node that has a list of it's chirldren.
data Heap a = EmptyH | Node a [Heap a]
                  deriving (Show, Read)

instance Eq a => Eq (Heap a) where
  (==) (EmptyH) (EmptyH) = True
  (==) (EmptyH) _ = False
  (==) _ (EmptyH) = False
  (==) (Node a as) (Node b bs) = a==b && as == bs

clearH a = EmptyH

emptyHe = EmptyH

isEmptyHH EmptyH = True
isEmptyHH _ = False

pushH x EmptyH = Node x []
pushH x y = pairMerge (Node x []) y

pairMerge :: (Ord a)=> Heap a -> Heap a -> Heap a
pairMerge EmptyH ha = ha
pairMerge ha EmptyH = ha
pairMerge (Node ha has) (Node hb hbs) =
  if (ha < hb)
    then Node ha ((Node hb hbs):has)
  else
     Node hb ((Node  ha has):hbs)

topH EmptyH = Nothing
topH (Node a as) =Just a

popH EmptyH = Nothing
popH (Node a as)=Just (a, merge as )

merge :: (Ord a)=> [Heap a] -> Heap a
merge []= EmptyH
merge (x:[]) = x
merge (x:y:[]) = pairMerge x y
merge (x:y:z) = pairMerge (pairMerge x y) (merge z)

makeHeapFromArray []=EmptyH
makeHeapFromArray (a:[]) =pushH a EmptyH
makeHeapFromArray (a:as) = pushH a (makeHeapFromArray as)

makeArrayFromHeap EmptyH = []
makeArrayFromHeap a = fst(extractMaybe(popH a)): makeArrayFromHeap(snd (extractMaybe(popH a)))

heapSort [] = []
heapSort a=makeArrayFromHeap(makeHeapFromArray a)

-- function taken from lab 6
extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just a) = a
