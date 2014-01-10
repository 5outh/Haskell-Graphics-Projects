module Utils(
  assignMaybe,
  maybeRead,
  toGLPoint,
  swapCoords
) where
  
import Types

import Data.IORef
import Data.Maybe(listToMaybe)
import Graphics.Rendering.OpenGL.Raw.Core

assignMaybe :: IORef a -> Maybe a -> IO ()
assignMaybe a (Just b) = a $= b
assignMaybe a Nothing  = return ()

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

toGLPoint :: IntPoint -> GLPoint
toGLPoint (a, b) = (fromIntegral a, fromIntegral b)

swapCoords :: (a, b) -> (b, a)
swapCoords (a, b) = (b, a)