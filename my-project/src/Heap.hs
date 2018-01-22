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
clearH    :: Heap a->Heap a
emptyHe    :: Heap a
isEmptyHH  :: Heap a -> Bool
pushH     :: (Ord a)=>a -> Heap a -> Heap a
popH      :: (Ord a)=>Heap a -> (Maybe a, Heap a)
topH      :: Heap a -> Maybe a
heapSort  :: (Ord a)=>[a]->[a]

--implementation

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

popH EmptyH = (Nothing, EmptyH)
popH (Node a as)=
  (Just a, merge as )

merge :: (Ord a)=> [Heap a] -> Heap a
merge []= EmptyH
merge (x:[]) = x
merge (x:y:[]) = pairMerge x y
merge (x:y:z) = pairMerge (pairMerge x y) (merge z)

makeHeapFromArray :: (Ord a)=> [a]-> Heap a
makeHeapFromArray []=EmptyH
makeHeapFromArray (a:[]) =pushH a EmptyH
makeHeapFromArray (a:as) = pushH a (makeHeapFromArray as)

makeArrayFromHeap :: (Ord a)=> Heap a -> [a]
makeArrayFromHeap EmptyH = []
makeArrayFromHeap a = extractMaybe(fst (popH a)): makeArrayFromHeap(snd (popH a))

heapSort [] = []
heapSort a=makeArrayFromHeap(makeHeapFromArray a)

extractMaybe :: Maybe a -> a
extractMaybe Nothing  = error "Nothing inside!"
extractMaybe (Just x) = x

--let r1 = Node 5 [Node 7 [], Node 8 [], Node 2 [], Node 4 [Node 7 []]]
