module Utils where

import           Data.Word

{-|
Make compact index of input ASCII set
a-z => 1-26, A-Z => 27-52 => 0-9: 53-62
-}
compactWord :: Word8 -> Word8
compactWord c
  | 97 <= c && c <= 122 = c - 96
  | 65 <= c && c <= 90 = c - (64 - 26)
  | otherwise = c + (52 - 47)

{-|
Revert compact index to ASCII index
a-z <= 1-26, A-Z <= 27-52, 0-9 <= 53-62
-}
revertWord :: Word8 -> Word8
revertWord c
  | 1 <= c && c <= 26 = c + 96
  | 27 <= c && c <= 52 = c + (64 - 26)
  | otherwise = c - (52 - 47)

{-|
Type cast Word8 to Char
-}
wordToChar :: Word8 -> Char
wordToChar = toEnum . fromEnum

{-|
Simple Binary Search Tree.

Each node holds the pair of next character and state index.
This structure will remain only on the constructing tree phases.
-}
data BSTree a x
  = BSNode a x (BSTree a x) (BSTree a x)
  | BSLeaf
  deriving (Show)

{-|
Insert new (key, value) to a tree, and return new inserted tree.
-}
bstInsert :: Ord a => BSTree a x -> a -> x -> BSTree a x
bstInsert tree a x = case tree of
  BSNode a' x' l r ->
    if a < a'
      then BSNode a' x' (bstInsert l a x) r
      else
        if a > a'
          then BSNode a' x' l (bstInsert r a x)
          else BSNode a x l r
  BSLeaf -> BSNode a x BSLeaf BSLeaf

{-|
Search a value from tree with a given key.
If there is no key, return Nothing
-}
bstSearch :: Ord a => BSTree a x -> a -> Maybe x
bstSearch tree a = case tree of
  BSNode a' x' l r ->
    if a < a'
      then bstSearch l a
      else
        if a > a'
          then bstSearch r a
          else Just x'
  BSLeaf -> Nothing

{-|
Return (index, value) list with inorder traversal.
-}
bstToList :: BSTree a x -> [(a, x)]
bstToList tree = toList tree []
  where
    toList :: BSTree a x -> [(a, x)] -> [(a, x)]
    toList tree res =
      case tree of
        BSLeaf         -> res
        BSNode a b l r -> toList r (toList l ((a, b) : res))

{-|
Queue with double linked-list
See Purely Functional Data Structures (Okazaki 98')

push, pop cost: amortized O(1)
|-}
data Queue a = Queue
  { _head :: [a],
    _tail :: [a]
  }
  deriving (Show)

{-|
Make new empty queue.
-}
newQueue :: Queue a
newQueue = Queue [] []

{-|
O(1). Insert given element to the end of queue and return inserted queue.
-}
enqueue :: Queue a -> a -> Queue a
enqueue (Queue h t) a = Queue h (a : t)

{-|
O(m). Insert every element in the list and return inserted queue.
-}
enqueueAll :: Queue a -> [a] -> Queue a
enqueueAll = foldr (flip enqueue)

{-|
Amortized O(1). remove the first element and return removed queue.
If queue was empty, return Nothing.
-}
dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue [] [])    = (Nothing, q)
dequeue (Queue [] t)       = dequeue (Queue (reverse t) [])
dequeue (Queue (h : t) t') = (Just h, Queue t t')

{-|
Check whether the queue is empty.
-}
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

{-|
Merge two list sets which are sorted in ascending order.
-}
mergeListSet :: (Ord a) => [a] -> [a] -> [a]
mergeListSet [] [] = []
mergeListSet [] l = l
mergeListSet l [] = l
mergeListSet al@(a : at) bl@(b : bt)
  | a < b = a : mergeListSet at bl
  | a > b = b : mergeListSet al bt
  | otherwise = a : mergeListSet at bt

{-|
Get a length and (index, value) list which is sorted in descending order.
Return new list with a given length according to given index/value list.

If there are no corresponding index, the result will be filled with zero.
Index is 1-based.
-}
fillZero :: Int -> [(Int, Int)] -> [Int]
fillZero len list = fill len list []
  where
    fill len _ ret | len <= 0 = ret
    fill len [] ret = fill (len - 1) [] (0 : ret)
    fill len l@((i, v) : t) ret
      | i < len = fill (len - 1) l (0 : ret)
      | otherwise = fill (len - 1) t (v : ret)

-- $> fillZero 5 [(5, 1), (3, 2), (2, 66)]
-- [0, 66, 2, 0, 1]
