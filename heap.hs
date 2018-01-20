module Heap
  ( Heap
  , clearH
  , isEmptyH
  , pushH
  , popH
  , topH
  , heapSort
  ) where

--interfaces
clearH    :: Heap a->Heap a
isEmptyH  :: Heap a -> Bool
pushH     :: (Ord a)=>a -> Heap a -> Heap a
popH      :: (Ord a)=>Heap a -> (Maybe a, Heap a)
topH      :: Heap a -> Maybe a
heapSort  :: (Ord a)=>[a]->[a]

--implementation

data Heap a = Empty | Node a [Heap a]
                  deriving Show

clearH a = Empty
isEmptyH Empty = True
isEmptyH _ = False

pushH x Empty = Node x []
pushH x y = pairMerge (Node x []) y

pairMerge::(Ord a)=> Heap a -> Heap a -> Heap a
pairMerge Empty ha = ha
pairMerge ha Empty = ha
pairMerge (Node ha has) (Node hb hbs) =
  if (ha < hb)
    then Node ha ((Node hb hbs):has)
  else
     Node hb ((Node  ha has):hbs)

topH Empty = Nothing
topH (Node a as) =Just a

popH Empty = (Nothing, Empty)
popH (Node a as)=
  (Just a, merge as )

merge::(Ord a)=> [Heap a] -> Heap a
merge []= Empty
merge (x:[]) = x
merge (x:y:[]) = pairMerge x y
merge (x:y:z) = pairMerge (pairMerge x y) (merge z)

makeHeapFromArray :: (Ord a)=> [a]-> Heap a
makeHeapFromArray []=Empty
makeHeapFromArray (a:[]) =pushH a Empty
makeHeapFromArray (a:as) = pushH a (makeHeapFromArray as)

makeArrayFromHeap :: (Ord a)=> Heap a -> [a]
makeArrayFromHeap Empty = []
makeArrayFromHeap a = extractMaybe(fst (popH a)): makeArrayFromHeap(snd (popH a))

heapSort [] = []
heapSort a=makeArrayFromHeap(makeHeapFromArray a)
extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x

--let r1 = Node 5 [Node 7 [], Node 8 [], Node 2 [], Node 4 [Node 7 []]]
