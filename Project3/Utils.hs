module Utils(
  assignMaybe,
  maybeRead,
  toGLPoint,
  swapCoords,
  toMatrix,
  toMatrix3d,
  toGLPoint,
  toGLPoint3d
) where
  
import Types

import Data.IORef
import Data.Maybe(listToMaybe)
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.StateVar ( ($=), ($=!), get )

assignMaybe :: IORef a -> Maybe a -> IO ()
assignMaybe a (Just b) = a $= b
assignMaybe a Nothing  = return ()

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

swapCoords :: (a, b) -> (b, a)
swapCoords (a, b) = (b, a)

--2d point to vertical matrix
toMatrix :: (Num a) => IntPoint -> Matrix a
toMatrix (x, y) = Matrix 3 1 [[fromIntegral x], [fromIntegral y], [fromIntegral 1]]

toMatrix3d :: (Num a) => IntPoint3d -> Matrix a
toMatrix3d (x, y, z) = Matrix 4 1 [[fromIntegral x], [fromIntegral y], [fromIntegral z], [fromIntegral 1]]

toGLPoint :: IntPoint -> GLPoint
toGLPoint (a, b) = (fromIntegral a, fromIntegral b)

toGLPoint3d :: IntPoint3d -> GLPoint3d
toGLPoint3d (a, b, c) = (fromIntegral a, fromIntegral b, fromIntegral c)