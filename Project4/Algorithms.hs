module Algorithms(
  cross,
  (<->), (<+>), (<*>),
  smult,
  negatePoint3d,
  translate,
  scale,
  rotate,
  normal,
  magnitude,
  (<>), (<>>), (=>>),
  applyMatrix,
  applyMatrix3d,
  applyMatrixToLine,
  applyMatrixToLine3d,
  cohenSutherland,
  bresenham,
  drawPoints,
  randomShortLine,
  randomLongLine,
  projectLinesForRendering,
  aboutPoint,
  aboutPoint3d,
  aboutCenter,
  aboutCenter3d
) where

import Types
import Utils

import Data.List(transpose, sort, unfoldr)
import Data.Bits((.|.), (.&.))
import System.Random(getStdRandom, randomR)
import Data.Maybe(catMaybes)

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

{- START PERSPECTIVE PROJECTION -}

persp near far = Matrix 4 4 [
  [1, 0, 0, 0],
  [0, 1, 0, 0],
  [0, 0, 1/(k-1), k/(k-1)],
  [0, 0, -1, 0]
  ]
  where k = near / far
  
scl w h far = Matrix 4 4 [
  [(h/w)/far, 0, 0, 0],
  [0, 1/far, 0, 0],
  [0, 0, 1/far, 0],
  [0, 0, 0, 1]
  ]

rot eye up = Matrix 4 4 [
  [rx, ry, rz, 0],
  [ux, uy, uz, 0],
  [nx, ny, nz, 0],
  [0,   0,  0, 1]
  ]
    where n@(nx, ny, nz) = smult ((-1) / (magnitude eye)) eye --(0, 0, 0), redundant
          r@(rx, ry, rz) = smult (1 / (magnitude un)) un
              where un = up `cross` n
          u@(ux, uy, uz) = n `cross` r

trans eye@(ex, ey, ez) = Matrix 4 4 [
  [1, 0, 0, -ex],
  [0, 1, 0, -ey],
  [0, 0, 1, -ez],
  [0, 0, 0,   1]
  ]

aboutPoint   p@(x, y)    m = translateMatrix x y <> m <>> translateMatrix (-x) (-y)
aboutPoint3d p@(x, y, z) m = translate3d x y z <> m <>> translate3d (-x) (-y) (-z)

aboutCenter   w h m = aboutPoint   (w/2, h/2)    m
aboutCenter3d w h m = aboutPoint3d (w/2, h/2, 0) m
        
projectrix near far w h eye up = (persp near far) <> (scl w h far) <>> (rot eye up) <>> (trans eye)

projectPoint near far width height eye up pt = pt2d
  where (Just p) = projectrix near far width height eye up
        (Just pt'@(Matrix _ _ [[u],[v],[d],[w']])) = p <> toMatrix3d pt
        pt2d = (width * (u / (w'+1))/2 , height * (v/(w'+1))/2)
 
projectLine' near far width height eye up (Line3d p1 p2) = Line (f p1) (f p2)
  where f = projectPoint near far width height eye up

projectLinesForRendering' :: (Floating a, RealFrac a) => a -> a -> a -> a -> Point3d a -> Point3d a -> [Line3d a] -> [Line Int]  
projectLinesForRendering' near far width height eye up ls = ls'
  where ls' = map (fmap floor) $ fmap (norm . projectLine' near far width height eye up) ls
        norm (Line (x1, y1) (x2, y2)) = Line (x1 * width + width/2, y1 * height + height/2) (x2 * width + width/2, y2 * height + height/2)
        
-- A one-stop shop for handling everything. Converts a list of 3d points to 2d ones, ready to be drawn.
-- Is `norm` correct? Who the hell knows. But it works okay?
projectLinesForRendering dist ssize width height eye ls = projectLinesForRendering' near far width height eye up ls
  where near = dist
        far = dist * height / ssize
        up = (0, 1, 0)

{- END PERSPECTIVE PROJECTION -}

cross :: (Num a) => Point3d a -> Point3d a -> Point3d a
cross (u1, u2, u3) 
      (v1, v2, v3) = (x, y, z)
       where x = (u2 * v3) - (u3 * v2)
             y = - ( (u1 * v3) - (u3 * v1) )
             z = (u1 * v2) - (u2 * v1)

(<+>) :: (Num a) => Point3d a -> Point3d a -> Point3d a
(u1, u2, u3) <+> (v1, v2, v3) = (u1 + v1, u2 + v2, u3 + v3)

negatePoint3d u = smult (-1) u

u <-> v = u <+> (negatePoint3d v)

magnitude (u1, u2, u3) = sqrt (u1^2 + u2^2 + u3^2)

smult s (x, y, z) = (s * x, s * y, s * z)

normal u = smult (1 / (magnitude u)) u

(u1, u2, u3) <*> (v1, v2, v3) = u1 * v1 + u2 * v2 + u3 * v3

getX, getY, getZ :: Point3d a -> a
getX (x, _, _) = x
getY (_, y, _) = y
getZ (_, _, z) = z


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

-- Apply m2, then m1 
Just m1 <>> m2 = m1 <> m2
Nothing <>> _  = Nothing

-- Apply m1, then m2 (clearer when chaining transformations)
Just m1 =>> m2 = m2 <> m1
Nothing =>> _  = Nothing

applyMatrix :: (Num a, RealFrac a) => IntPoint -> Matrix a -> Maybe IntPoint
applyMatrix pt m = 
  let converted = m <> (toMatrix pt) in 
    case converted of
      Just m' -> let [[x], [y], _] = mdata m' in Just (floor x, floor y)
      _ -> Nothing

applyMatrix3d :: (Num a, RealFrac a) => Point3d a -> Matrix a -> Maybe (Point3d a)
applyMatrix3d pt m =
  let converted = m <> (toMatrix3d pt) in 
    case converted of
      Just m' -> let [[x], [y], [z], _] = mdata m' in Just (x, y, z)
      _       -> Nothing

applyMatrixToLine m (Line p1 p2) = case (x', y') of
                                     (Just x, Just y) -> Line x y
                                     _                -> Line p1 p2
  where (x', y') = ((applyMatrix p1 m), (applyMatrix p2 m))

applyMatrixToLine3d m (Line3d p1 p2) = case (x', y') of
                                        (Just x, Just y) -> Line3d x y
                                        _                -> Line3d p1 p2
  where (x', y') = ((applyMatrix3d p1 m), (applyMatrix3d p2 m)) 

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

inside = 0; -- 0000
left = 1;   -- 0001
right = 2;  -- 0010
bottom = 4; -- 0100
top = 8;    -- 1000                           
                           
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
       
perspectiveProject :: GLPoint3d -> GLPoint3d -> GLPoint3d -> GLfloat -> GLfloat -> Int -> Int -> Int -> GLPoint
perspectiveProject 
  point@(p1, p2, p3) focus@(f1, f2, f3) trp@(t1, t2, t3)
  s d coord width height = (px, py)
    where wcs = s / d
          (cx, cy, cz) = (cos t1, cos t2, cos t3)
          (sx, sy, sz) = (sin t1, sin t2, sin t3)
          (ddx, ddy, ddz) = point <-> focus
          s_yz1 = cy * ddz + sy * (sz * ddy + cz * ddx)
          s_yz2 = cz * ddy - sz * ddx
          dx = cy * (sz * ddy + cz * ddx) - sy * ddz
          dy = sx * s_yz1 + cx * s_yz2
          dz = cx * s_yz1 - sx * s_yz2
          px = (fromIntegral $ width `div` 2) + dx * 2 / dz * wcs * (fromIntegral coord)
          py = (fromIntegral $ height `div` 2) + dx * 2 / dz * wcs * (fromIntegral coord)

projectLine f t s d c w h (Line3d a b) = Line (perspectiveProject a f t s d c w h) (perspectiveProject a f t s d c w h)

{-testCube = fmap (projectLine focus trp s d coord w h) cube
  where focus = (6, 8, 7.5)
        trp   = (0, 0, 0)
        s     = 30
        d     = 150
        coord = 400
        w     = 400
        h     = 400        
        
test = perspectiveProject point focus trp s d coord w h
  where focus = (6, 8, 7.5)
        point = (1, 1, 1)
        trp   = (0, 0, 0)
        s     = 30
        d     = 60
        coord = 400
        w     = 400
        h     = 400
-}
          
drawPoints xs = do
 glBegin gl_POINTS
 --glColor3f 1 1 1
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