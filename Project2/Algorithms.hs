module Algorithms(
  translate,
  scale,
  rotate,
  (<>), (<>>),
  applyMatrix,
  applyMatrixToLine,
  cohenSutherland,
  bresenham,
  drawPoints,
  randomShortLine,
  randomLongLine
) where

import Types
import Utils

import Data.List(transpose, sort, unfoldr)
import Data.Bits((.|.), (.&.))
import System.Random(getStdRandom, randomR)
import Data.Maybe(catMaybes)

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

translate :: IntPoint -> Int -> Int -> IntPoint
translate (x, y) tx ty = (x + tx, y + ty)

scale :: IntPoint -> Int -> Int -> IntPoint
scale (x, y) sx sy = (x * sx, y * sy)

rotate :: IntPoint -> Float -> IntPoint
rotate (x, y) angle = (x', y')
  where rads = cos (pi * angle / 180)
        x' = floor $ (fromIntegral x) * (sin rads - cos rads)
        y' = floor $ (fromIntegral y) * (cos rads - sin rads)

(<>) :: (Num a) => Matrix a -> Matrix a -> Maybe (Matrix a)
(Matrix h w xs) <> (Matrix h' w' ys) 
  | w == h' = Just $ Matrix h w' $ fmap (\x-> fmap (sum . zipWith (*) x) (transpose ys)) xs
  | otherwise = Nothing

Just m1 <>> m2 = m1 <> m2
Nothing <>> _  = Nothing

applyMatrix :: (Num a, RealFrac a) => IntPoint -> Matrix a -> Maybe IntPoint
applyMatrix pt m = 
  let converted = m <> (toMatrix pt) in 
    case converted of
      Just m' -> let [[x], [y], _] = mdata m' in Just (floor x, floor y)
      _ -> Nothing

applyMatrixToLine m (Line p1 p2) = case (x', y') of
                                     (Just x, Just y) -> Line x y
                                     _                -> Line p1 p2
  where (x', y') = ((applyMatrix p1 m), (applyMatrix p2 m))
      
inside = 0; -- 0000
left = 1;   -- 0001
right = 2;  -- 0010
bottom = 4; -- 0100
top = 8;    -- 1000

--draw a line from a to b using bresenham
bresenham :: Maybe Int -> Line Int -> IO ()
bresenham stripe (Line a@(x0, y0) b@(x1, y1)) = 
  drawPoints $ catMaybes $ unfoldr go (err, x0', y0')
  where
    skip x = case stripe of
                  (Just s) -> x+1 `mod` s == 0 || x `mod` s == 0
                  _        -> False
    fixIfSteep (a, b) = if steep then (b, a) else (a, b)
    steep = abs (x1-x0) < abs (y1-y0)
    [(x0',y0'), (x1',y1')] = sort $ fmap fixIfSteep [a, b]
    (dx, dy) = (x1'-x0', abs $ y1'-y0')
    err = dx `div` 2
    ystep = if y0' < y1' then 1 else -1
    go (err, x, y)
      | x > x1'  = Nothing
      | otherwise = Just $ (point, (err'', x+1, y'))
        where err' = err - dy
              (err'', y') | err' < 0  = (err' + dx, y + ystep)
                          | otherwise = (err', y)
              point = case skip x of 
                           False -> Just $ toGLPoint $ fixIfSteep (x, y)
                           True  -> Nothing

-- Produces a line clipped across the viewport if one exists. If it does not intersect, returns Nothing
cohenSutherland :: Viewport -> Line Int -> Maybe (Line Int)
cohenSutherland v@(Viewport minx miny maxx maxy) l@(Line p1@(x1, y1) p2@(x2, y2)) =
  if not (bitTest (oc1 .|. oc2)) then Just l  -- completely inside viewport
  else if bitTest (oc1 .&. oc2)  then Nothing -- completely outside viewport
  else cohenSutherland v (newLine clipIntPoint)  -- partially inside viewport
  where [oc1, oc2] = fmap (getOutCode v) [p1, p2]
        outcode    = max oc1 oc2 --one is 0; choose the non-zero one
        newLine p  = if outcode == oc1 then Line p p2 else Line p1 p
        bitTest x = x /= 0
        -- compute intersection IntPoint
        clipIntPoint 
          | bitTest (outcode .&. top) = 
              let x' = x1 + (x2 - x1) * (maxy - y1) `div` (y2 - y1) in (x', maxy)
          | bitTest (outcode .&. bottom) =
              let x' = x1 + (x2 - x1) * (miny - y1) `div` (y2 - y1) in (x', miny)
          | bitTest (outcode .&. right) =
              let y' = y1 + (y2 - y1) * (maxx - x1) `div` (x2 - x1) in (maxx, y')
          | bitTest (outcode .&. left) =
              let y' = y1 + (y2 - y1) * (minx - x1) `div` (x2 - x1) in (minx, y')
          | otherwise = error "OutCode error : BitTest is not working."
        --getOutCode is local to cohen-sutherland
        getOutCode :: Viewport -> IntPoint -> Int
        getOutCode (Viewport minx miny maxx maxy) (x, y) = (handleX . handleY) inside
          where handleX code | x < minx = code .|. left --left
                             | x > maxx = code .|. right --right
                             | otherwise = code
                handleY code | y < miny = code .|. bottom --bottom
                             | y > maxy = code .|. top --top
                             | otherwise = code
                             
drawPoints xs = do
 glBegin gl_POINTS
 glColor3f 1 1 1
 mapM_ (uncurry glVertex2f) xs
 glEnd

randomShortLine alg = do
   x <- getStdRandom $ randomR (10, 630)
   y <- getStdRandom $ randomR (10, 630)
   dx <- getStdRandom $ randomR (-10, 10)
   dy <- getStdRandom $ randomR (-10, 10)
   alg $ Line (x, y) (x + dx, y + dy)

randomLongLine alg = do
   x <- getStdRandom $ randomR (0, 640)
   y <- getStdRandom $ randomR (0, 640)
   dx <- getStdRandom $ randomR (-640, 640)
   dy <- getStdRandom $ randomR (-640, 640)
   alg $ Line (x, y) (x + dx, y + dy)