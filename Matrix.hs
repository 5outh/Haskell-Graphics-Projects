import Data.List(transpose)
import Numeric.LinearAlgebra.Algorithms

data Matrix a = Matrix { width :: Int, height :: Int, array :: [a] } deriving (Show, Eq)

mkMatrix :: (Num a) => Int -> Int -> [a] -> Matrix a
mkMatrix w h a = Matrix w h (take (w*h) (a ++ repeat 0))

--add :: (Num a, Num b, Num c) => Matrix a -> Matrix b -> Maybe (Matrix c)
add (Matrix w h a) (Matrix w' h' a') 
  | w == w' && h == h' = Just $ Matrix w h (zipWith (+) a a')
  | otherwise = Nothing


multiply (Matrix w h a) (Matrix w' h' a')
  | w == h' = undefined --normal
  | otherwise = Nothing

groupN n xs@(_:_) = (take n xs) : groupN n (drop n xs)
groupN n [] = []

transpose' :: [[a]] -> [[a]]
transpose' xs@(x:_)
  | null x = []
  | otherwise = heads : transpose' tails
    where heads = fmap head xs
          tails = fmap tail xs