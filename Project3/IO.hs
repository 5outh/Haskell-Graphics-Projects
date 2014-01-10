module IO(
  readLinesFromFile,
  writeLinesToFile
) where

import Types

import System.IO(hGetContents, IOMode(..), openFile, writeFile)
import Data.List(intercalate)
import Data.List.Split(splitOn)

readLines = fmap readLine . lines

readLinesFromFile filename = do
  handle <- openFile filename ReadMode  
  contents <- hGetContents handle
  return $ readLines contents

readLine str = 
  let [x0, y0, x1, y1] = fmap readInt $ splitOn " " str 
  in Line (x0, y0) (x1, y1)
  where readInt x = read x :: Int

lineToString (Line p1@(x0, y0) p2@(x1, y1)) = intercalate " " $ fmap show [x0, y0, x1, y1]

writeLinesToFile filename lns = do
  let str = intercalate "\n" $ fmap lineToString lns
  writeFile filename str