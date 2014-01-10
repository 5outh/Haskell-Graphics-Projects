{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Bits
import Data.Word
import Data.List(foldr1)
import Control.Monad(foldM)

setLast 1 = flip setBit   0
setLast 0 = flip clearBit 0

getBits = reverse . getBits'

getBits' 1 = [1]
getBits' 0 = [0]
getBits' n = case testBit n 0 of
	True -> 1 : getBits' (shiftR n 1)
	_ 	 -> 0 : getBits' (shiftR n 1)

setsLast xs a = zipWith setLast (getBits a) xs 