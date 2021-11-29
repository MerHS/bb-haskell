{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Criterion.Main
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (readInt)
import           Data.String
import           Data.Word             (Word8)
import           System.Environment
import           Text.Printf

import           BakerBird


-- strip whitespace
strip :: B.ByteString -> B.ByteString
strip = B.filter (/= 13)

parseFile :: String -> IO (Int, Int, [B.ByteString], [B.ByteString])
parseFile inputFilePath = do
  input <- B.readFile inputFilePath
  let
    nums:lines = B.split 10 input -- fromEnum '\n' == 10
    mStr:[nStr] = B.split 32 nums -- fromEnum ' ' == 32

    n, m :: Int
    Just (m, _) = readInt mStr
    Just (n, _) = readInt nStr

    (patL, dataL) = splitAt m lines
    (patS, dataS) = (map strip patL, map strip dataL)

  return (m, n, patS, dataS)

type BenchFun = (Int, Int) -> [(Int, Int)]

benchFunGen :: String -> IO [(Int, Int)]
benchFunGen inputFilePath = do
  (m, n, patS, dataS) <- parseFile inputFilePath
  return $ bakerBird (m, n) patS dataS

main :: IO ()
main = do
  inputFilePath:args <- getArgs

  withArgs args $ defaultMain [
    bgroup "baker-bird"
    [ bench "bench" $ nfIO $ benchFunGen inputFilePath ]
    ]


