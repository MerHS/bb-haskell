{-# LANGUAGE OverloadedStrings #-}
module Main where

-- From this project, I only used three external dependencies:
--   `bytestring`: efficiently manipulate string data.
--   `array`: builds O(1) indexing array.
--   `criterion`: measure performance of function execution
--
-- This dependencies can be found from package.yaml file
-- which configures Haskell package manager `stack`.

import           Control.Monad
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

-- parse input file and return m, n, pattern text and search target text.
parseFile :: String -> IO (Int, Int, [B.ByteString], [B.ByteString])
parseFile inputFilePath = do
  -- read texts from input
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

main :: IO ()
main = do
  -- get commandline arguments
  inputFilePath:[outputFilePath] <- getArgs

  -- parse input
  (m, n, patS, dataS) <- parseFile inputFilePath

  let
    -- compute baker-bird
    results = bakerBird (m, n) patS dataS

    -- convert the output result as string
    strMap = map (uncurry (printf "%d %d")) results
    resultStr = B.intercalate "\n" (map fromString strMap)

  B.writeFile outputFilePath resultStr
  putStrLn "finished"
