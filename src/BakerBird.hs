{-# LANGUAGE OverloadedStrings #-}
module BakerBird where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString  as B
import           Data.List

import           AhoCorasick
import           KMP
import           Utils

{-|
Take a list of values, and return a list of distinct values and its indices.
Indices are one-based.
-}
distinctElem :: Eq a => [a] -> ([a], [Int])
distinctElem as = distinctRow_ as [] [] 0 where
  distinctRow_ [] rl rIdx _ = (reverse rl, reverse rIdx)
  distinctRow_ (a:as) rl rIdx idxLen =
    case elemIndex a rl of
      Just i  -> distinctRow_ as rl ((idxLen - i):rIdx) idxLen
      Nothing -> distinctRow_ as (a:rl) ((idxLen + 1):rIdx) (idxLen + 1)

-- $> distinctElem [1, 2, 4, 5, 2, 5] == ([1, 2, 4, 5], [1, 2, 3, 4, 2, 4])

{-|
Take (pattern length, data length), pattern.
Return a list of found indices
-}
bakerBird :: (Int, Int) -> [B.ByteString] -> [B.ByteString] -> [(Int, Int)]
bakerBird (m, n) pat text =
  -- feed each text row to main loop body
  concat . reverse $ snd $ foldl' consumeRow (initKmpStates, []) (zip [1..] text)
  where
    -- filter distinct elements (O(m^2))
    (distinctPat, rArr) = distinctElem pat

    -- initial AC state and pattern matching machine
    -- space: O(|\Sigma|m^2),
    acState = genFailure $ buildPMM distinctPat

    -- replicate KMP state and assign to each column of target text
    -- preprocessing time: O(m), KMP space: O(m), column list: O(n)
    --
    -- KMP string and failure function are shared across columns
    -- so only O(n) extra spaces for state indices are needed.
    initKmpStates = replicate n . genKMPState . toUArray $ rArr

    -- main loop body
    -- kmpS: KMP states of each column
    -- ret: result list
    -- rowIdx, str: current row index and text line
    consumeRow (kmpS, ret) (rowIdx, str) =
      let
        -- collected AC state (O(n))
        acRow = fillZero n $ searchTextAC acState str
        -- update each KMP state by computed AC state (O(n))
        kmpThreads = zipWith nextKMPState kmpS acRow
        -- collect column and row indices (O(n))
        rowResult = map (\(colIdx, _) -> (rowIdx-1, colIdx-1)) $ filter snd (zip [1..] (map snd kmpThreads))
      in
        (map fst kmpThreads, rowResult:ret)
