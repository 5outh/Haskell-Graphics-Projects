module Types(
  Matrix(..),
  Point(..),
  IntPoint,
  GLPoint,
  Line(..),
  Viewport(..),
  mkViewport,
  translateMatrix,
  scaleMatrix,
  rotationMatrix
) where

import Graphics.Rendering.OpenGL.Raw.Core31

data Matrix a = Matrix{ w :: Int, h :: Int, mdata :: [[a]] } deriving (Show, Eq)
type Point a  = (a, a)
type IntPoint = Point Int
type GLPoint  = Point GLfloat

data Line a = Line {p_x :: Point a, p_y :: Point a} deriving (Show, Eq)
data Viewport = Viewport {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving (Show, Eq)

--Safe making viewport
mkViewport :: Int -> Int -> Int -> Int -> Maybe Viewport
mkViewport minx miny maxx maxy 
  | minx < maxx && miny < maxy = Just (Viewport minx miny maxx maxy)
  | otherwise = Nothing

translateMatrix tx ty = Matrix 3 3 [[1, 0, tx], [0, 1, ty], [0, 0, 1]]
scaleMatrix sx sy     = Matrix 3 3 [[sx, 0, 0], [0, sy, 0], [0, 0, 1]]
rotationMatrix theta  = Matrix 3 3 [[cos t, -(sin t), 0], [sin t, cos t, 0], [0, 0, 1]]
  where t =  - (theta * pi / 180) -- to change to clockwise rotation and map to rads