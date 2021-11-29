{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-|
Knuth-Morris-Pratt Algorithm on UArray.
-}
module KMP where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array         as A
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Foldable

{-|
KMP state machine.
-}
data KMPState a = KMPState {
  stateIdx :: Int, -- ^ current state index
  string   :: UArray Int a, -- ^ pattern string
  failure  :: UArray Int Int -- ^ failure function (as array)
  }

{-|
Make KMP state machine from given pattern string.
-}
genKMPState :: (IArray UArray a, Eq a) => UArray Int a -> KMPState a
genKMPState pat =
  let (start, end) = bounds pat
      zero = start - 1
  in
  -- return new state.
  -- failure function is the result of runST action.
  KMPState zero pat $ runST $ do
    -- generate new array which is filled with zero
    arr <- newArray (start, end) zero :: ST s (STUArray s Int Int)

    let
      -- compute: recursive computing action.
      -- q: current index on string
      -- k: current state of state machine
      compute = \q k -> do
        when (q <= end) $ do
          if k > zero && (pat ! (k + 1)) /= (pat ! q) then do
            k <- readArray arr k
            compute q k
          else if (k > zero) || (pat ! (k + 1)) == (pat ! q) then do
            writeArray arr q (k + 1)
            compute (q + 1) (k + 1)
          else
            compute (q + 1) zero

    compute (start + 1) zero

    -- freeze array into immutable array and return it.
    freeze arr

{-|
Consume new character and return new state with occurrence.
-}
nextKMPState :: (IArray UArray a, Eq a) => KMPState a -> a -> (KMPState a, Bool)
nextKMPState (KMPState q pat f) a = search' q where
  (start, end) = bounds pat
  zero = start - 1
  -- main loop body
  search' q 
    | q > zero && (pat ! (q + 1)) /= a = search' $ f ! q
    | q > zero || (pat ! (q + 1) == a) =
      -- found new occurence
      if q + 1 == end then (KMPState (f ! (q + 1)) pat f, True)
      -- not found
      else (KMPState (q + 1) pat f, False)
    | otherwise = (KMPState zero pat f, False)


-- KMP searcher. Does not used in Baker-Bird. (for testing purpose)
searchKMP :: (IArray UArray a, Eq a) => UArray Int a -> UArray Int a -> [Int]
searchKMP pat text =
  snd $ foldl' consume (state, []) (assocs text)
  where
    (start, _) = bounds text
    state = genKMPState pat
    consume (s, res) (i, c) =
      let (nextS, found) = nextKMPState s c in
        (nextS, if found then i:res else res)


toUArray :: [Int] -> UArray Int Int
toUArray l = listArray (1, length l) l

-- $> searchKMP (toUArray [2,3,2]) $ toUArray [1,2,3,2,4,2,3,4,2,3,2,3,2]
-- [13, 11, 4]
