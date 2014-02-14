module Types(
  Matrix(..),
  Point(..),
  Point3d(..),
  IntPoint,
  IntPoint3d,
  GLPoint,
  GLPoint3d,
  Line(..),
  Line3d(..),
  Viewport(..),
  Axis(..),
  mkViewport,
  translateMatrix,
  scaleMatrix,
  rotationMatrix,
  translate3d,
  scale3d,
  rotate3d,
  cube,
  pmap,
  pmap3d,
  identity4
) where

import Graphics.Rendering.OpenGL.Raw.Core31

data Matrix a = Matrix{ w :: Int, h :: Int, mdata :: [[a]] } deriving (Show, Eq)
type Point a  = (a, a)
type Point3d a = (a, a, a)

type IntPoint = Point Int
type GLPoint  = Point GLfloat

type IntPoint3d = Point3d Int
type GLPoint3d = Point3d GLfloat

data Line a = Line {p_x :: Point a, p_y :: Point a} deriving (Show, Eq)
data Line3d a = Line3d { p :: Point3d a, q :: Point3d a} deriving (Show, Eq)

data Viewport = Viewport {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving (Show, Eq)

data Axis = X | Y | Z deriving (Show, Eq)

pmap   f (a, b)    = (f a, f b)
pmap3d f (a, b, c) = (f a, f b, f c)

instance Functor Line where
  fmap f (Line a b) = Line (pmap f a) (pmap f b)

instance Functor Line3d where
  fmap f (Line3d a b) = Line3d (pmap3d f a) (pmap3d f b)  

--Safe making viewport
mkViewport :: Int -> Int -> Int -> Int -> Maybe Viewport
mkViewport minx miny maxx maxy 
  | minx < maxx && miny < maxy = Just (Viewport minx miny maxx maxy)
  | otherwise = Nothing

translateMatrix tx ty = Matrix 3 3 [[1, 0, tx], [0, 1, ty], [0, 0, 1]]
scaleMatrix sx sy     = Matrix 3 3 [[sx, 0, 0], [0, sy, 0], [0, 0, 1]]
rotationMatrix theta  = Matrix 3 3 [[cos t, -(sin t), 0], [sin t, cos t, 0], [0, 0, 1]]
  where t =  - (theta * pi / 180) -- to change to clockwise rotation and map to rads

translate3d tx ty tz = Matrix 4 4 [[1, 0, 0, tx], [0, 1, 0, ty], [0, 0, 1, tz], [0, 0, 0, 1.0]]
scale3d     sx sy sz = Matrix 4 4 [[sx, 0, 0, 0], [0, sy, 0, 0], [0, 0, sz, 0], [0, 0, 0, 1.0]]
rotate3d    a theta  = 
  let t = - (theta * pi / 180)
      c = cos t
      s = sin t in Matrix 4 4 $ case a of
  X -> [[1, 0, 0, 0], [0, c, s, 0], [0, (-s), c, 0], [0, 0, 0, 1]]
  Y -> [[c, 0, (-s), 0], [0, 1, 0, 0], [s, 0, c, 0], [0, 0, 0, 1]]
  Z -> [[c, s, 0, 0], [(-s), c, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]

identity4 = Matrix 4 4 [[1, 0, 0, 0],[0, 1, 0, 0],[0, 0, 1, 0],[0, 0, 0, 1]]  
  
cube :: [Line3d Double]
cube = zipWith Line3d [a, b, c, d, e, f, g, h, a, b, c, d] 
                      [b, c, d, a, f, g, h, e, e, f, g, h]
  where a = (-1, 1, -1)
        b = (1, 1, -1)
        c = (1, -1, -1)
        d = (-1, -1, -1)
        e = (-1, 1, 1)
        f = (1, 1, 1)
        g = (1, -1, 1)
        h = (-1, -1, 1)
