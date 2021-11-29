
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module AhoCorasick where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString    as B
import           Data.Char
import qualified Data.List          as L
import           Data.STRef
import           Data.Word          (Word8)
import           Text.Printf

import           Utils

{-|
Construction node of Pattern Matching Machine

IMPORTANT: Each name is NOT a field name of this structure.
In fact, they are "getter method" of PMMNode, therefore we should
write like this: `nodeIndex node`, not `node.nodeIndex`
-}
data PMMNode = PMMNode {
  nodeIndex  :: Int, -- ^ state index
  nextStates :: BSTree Word8 Int -- ^ tree of next states
  }
  deriving (Show)

{-| Internal representation of PMM Tree (Arena-Allocated Tree)

This tree maps a state index (Int) to each node (Maybe PMMNode).

ST type represents that this tree is currently in a mutable state.

Type variable `s` is 'Phantom type', which means there are no physical value
assigned to that `s` type. Phantom type is used within type checking phase,
and restricts some useful constraints to the context.

From this context, `s` type meas `State Thread`, which can destructively
mutate this type only from do block which holds `s` type.

cf.) Haskell does not support incremental data structure like `vector` of C++.

     To mitigate this problem, the tree is implemented with
     Arena-allocated tree, which is usually used in Rust language
     to avoid its memory ownership checker.

Arena-allocated Tree:
  https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
-}
type PMMTree s = ST s (STArray s Int (Maybe PMMNode))

{-| Final representation of Pattern Matching Machine
--
-- `Array` and `UArray` type represent the array of contiguous values.
-- It takes two type parameters: The first type is an index type, and the latter is a value type
-}
data PMM = PMM {
  pmmLength    :: Int,
  -- ^ total state count of PMM
  pmmEdges     :: UA.UArray (Int, Word8) Int,
  -- ^ Map (state index, char) -> next state_index (-1: not allocated)
  pmmEdgeChars :: Array Int [Word8],
  -- ^ Map state index -> list of chars on next edges
  pmmOutputs   :: Array Int [Int]
  -- ^ Map state index -> output index (i.e. index of ByteString list) list
  }

-- implementation of toString (show) method of pattern matching machine
instance Show PMM where
  show pmm =
    "length: " ++ show (pmmLength pmm) ++ "\n" ++
    "edges: " ++ showList "\n" (filterEdges (pmmEdges pmm)) ++ "\n" ++
    "outputs: " ++ showList ", " (filterOutputs (pmmOutputs pmm)) ++ "\n"
    where
      showList :: Show a => String -> [a] -> String
      showList sep = L.intercalate sep . map show
      filterEdges :: UA.UArray (Int, Word8) Int -> [String]
      filterEdges edges =
        map (\((s, w), s') -> printf "(%d -> %d: %c)" s s' (wordToChar $ revertWord w)) $
        filter (\(_, st) -> st >= 0) (UA.assocs edges)
      filterOutputs outputs =
        filter (\(_, st) -> st /= []) (assocs outputs)

{-|
Failure function with array
-}
type FailureTable = UA.UArray Int Int

{-|
Final structure of Aho-Corisack State machine.
-}
data ACState = ACState PMM FailureTable deriving (Show)

{-|
Build Pattern Matching Machine based on the list of strings.
-}
buildPMM :: [B.ByteString] -> PMM
buildPMM pats = runST $ do
  let m = sum $ map B.length pats

  -- make new pattern matching tree (i.e. trie)
  tree <- newArray (0, m) Nothing :: PMMTree s
  -- map state index -> bytestring index
  outputs <- newArray (1, m) [] :: ST s (STArray s Int [Int])

  -- Make new 'mutable variable'.
  -- Thanks to Haskell's State Monad (STRef), this is magically possible.
  -- For now, just consider it as a pointer of an integer.
  lastTreeIdxRef <- newSTRef 1

  -- insert head node
  writeArray tree 0 $ Just $ PMMNode 0 BSLeaf

  -- main loop that iterates pattern strings
  forM_ (zip pats [1..]) $ \(str, patIdx) -> do
    Just headNode <- readArray tree 0

    -- define variables (current node, next node)
    currNodeRef <- newSTRef headNode

    let
      strLen = B.length str
      -- define new function (exactly, monadic action) that generates new state
      genNewState char = do
        PMMNode currIdx currNexts <- readSTRef currNodeRef
        lastIdx <- readSTRef lastTreeIdxRef

        -- generate new node and insert it into the last position of tree arena.
        let newNode = PMMNode lastIdx BSLeaf
        writeSTRef currNodeRef newNode
        writeArray tree lastIdx $ Just newNode
        writeSTRef lastTreeIdxRef (lastIdx + 1)

        -- update current node
        let newCurr = PMMNode currIdx (bstInsert currNexts char lastIdx)
        writeArray tree currIdx $ Just newCurr

    -- iterate each char
    forM_ (zip (B.unpack str) [1..]) $ \(char, charIdx) -> do
      PMMNode currIdx currNexts <- readSTRef currNodeRef

      case currNexts of
        -- there is no next states -> make new next state
        BSLeaf -> genNewState char
        _ ->
          case bstSearch currNexts char of
            -- next char is not connected to this state -> make new next state
            Nothing -> genNewState char
            -- found next state -> go to that state
            Just nextIdx -> do
              Just nextNode <- readArray tree nextIdx
              writeSTRef currNodeRef nextNode

      -- string ends
      when (charIdx == strLen) $ do
        -- then, update output
        PMMNode currIdx currNexts <- readSTRef currNodeRef
        currOutputs <- readArray outputs currIdx
        writeArray outputs currIdx (patIdx:currOutputs)

  pmmLen' <- readSTRef lastTreeIdxRef
  let pmmLen = pmmLen' - 1

  -- convert binary search tree to array table
  edges <- newArray ((0, 1), (pmmLen, 62)) (-1) :: ST s (STUArray s (Int, Word8) Int)
  edgeChars <- newArray (0, pmmLen) [] :: ST s (STArray s Int [Word8])

  forM_ [0..pmmLen] $ \stateIdx -> do
    -- read each node from arena
    Just (PMMNode nodeIdx nextStates) <- readArray tree stateIdx

    -- write edges and edgeChars
    forM_ (bstToList nextStates) $ \(char, nextIdx) -> do
      let word = compactWord char
      writeArray edges (stateIdx, word) nextIdx
      charList <- readArray edgeChars stateIdx
      writeArray edgeChars stateIdx (word:charList)

  -- freeze mutable arrays into immutable arrays
  edgesArr <- freeze edges
  edgeCharsArr <- freeze edgeChars
  outputArr <- freeze outputs

  -- return final result
  return $ PMM pmmLen edgesArr edgeCharsArr outputArr

{-|
Generate failure function and update output array
-}
genFailure :: PMM -> ACState
genFailure (PMM pmmLen edges edgeChars outputs) =
  uncurry ACState $ runST $ do
  -- clone outputs and make it mutable
  outputTemp <- thaw outputs :: ST s (STArray s Int [Int])
  failure <- newArray (0, pmmLen) 0 :: ST s (STUArray s Int Int)

  let
    nextStates = getNextStates 0
    stateQueue = enqueueAll newQueue nextStates

    -- computeFailure recursively pops current state index
    -- from given queue, and update failure value to failure function
    computeFailure = \q ->
      unless (isEmpty q) $ do

      -- Breath First Search with Queue
      let
        (Just (_, currIdx), tailQueue) = dequeue q
        nextStates = getNextStates currIdx

        -- insert every state index of next depth
        nextQueue = enqueueAll tailQueue nextStates

      currFailure <- readArray failure currIdx

      -- Run BFS
      forM_ nextStates $ \(char, nextIdx) -> do
        let
          -- define recursive loop that backtracks failure function
          failureLoop = \failIdx -> do
            let nextFailure = edges UA.! (failIdx, char)

            -- found edge -> update failure function
            if nextFailure > 0 then do
              writeArray failure nextIdx nextFailure

              -- update output function
              oldOutput <- readArray outputTemp nextFailure
              oldOutput2 <- readArray outputTemp nextIdx
              writeArray outputTemp nextIdx (mergeListSet oldOutput oldOutput2)
            -- not found edge -> backtrack failure function
            else
              when (failIdx > 0) $ do
                prevFailure <- readArray failure failIdx
                failureLoop prevFailure

        failureLoop currFailure

      -- compute failure function of the nodes in next depth
      computeFailure nextQueue

  computeFailure stateQueue

  -- freeze failure and output array
  failureArr <- freeze failure
  newOutput <- freeze outputTemp

  return (PMM pmmLen edges edgeChars newOutput, failureArr)
  where
    -- get next state indices and chars on edges of currIdx state
    getNextStates currIdx = map (\c -> (c, edges UA.! (currIdx, c))) (edgeChars ! currIdx)


{-|
Search every location of pattern string from given string.
Returns list of (location index, pattern index).

Both location and pattern index start from 1.
-}
searchTextAC :: ACState -> B.ByteString -> [(Int, Int)]
searchTextAC (ACState (PMM _ edge _ outputs) failure) str = concat $ search' 0 1 []
  where
  strLen = B.length str
  -- search': recursive function which runs AC algorithm
  -- state: current state index
  -- i: current text position
  -- res: result list which is returned at the end
  search' state i res
    | i > strLen = res
    | otherwise =
    let char = compactWord $ B.index str (i - 1)
        next = edge UA.! (state, char)
    in
    if next >= 0 then -- found edge
      let outs = outputs ! next in
      if outs /= [] then -- if output is set, prepend outputs to result list
        search' next (i + 1) $ map (i, ) outs:res
      else -- else, go to next state
        search' next (i + 1) res
    else if state == 0 then -- not found edge && state == 0 -> proceed
      search' state (i + 1) res
    else -- not found edge -> go to failure state
      search' (failure UA.! state) i res


{-|
Main Aho-Corasick algorithm on the list of ByteString.
-}
ahoCorasick :: [B.ByteString] -> B.ByteString -> [(Int, Int)]
ahoCorasick = searchTextAC . genFailure . buildPMM
