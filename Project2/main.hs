import Types
import Utils
import Algorithms
import IO

import Control.Monad
import System.Exit
import System.Environment
import Data.IORef
import Data.List.Split(splitOn)

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar ( ($=), ($=!), get )

import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

import Data.Maybe(catMaybes)

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize

   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 640 640
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   
   viewport <- newIORef (Viewport 0 0 640 640)
   lns      <- newIORef ([] :: [Line Int])
    
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= (display viewport lns)
      
   idleCallback $= Just (idle viewport lns)
   
   mainLoop

idle viewport lns = do
  processInput viewport lns
  postRedisplay Nothing

processInput viewport lns = do
  cmd <- getLine
  let cmd' = splitOn " " cmd
  if (null cmd') then return () else
    case head cmd' of
      "viewport"  -> let [x0, x1, y0, y1] = fmap (\x -> read x :: Int) (tail cmd') 
                     in viewport `assignMaybe` mkViewport x0 x1 y0 y1
      "line"      -> let [x0, y0, x1, y1] = fmap (\x -> read x :: Int) (tail cmd') 
                     in lns $= [Line (x0, y0) (x1, y1)]
      "readfile"  -> do
                        lns' <- readLinesFromFile $ (head . tail)  cmd'
                        lns $= lns'
      "writefile" -> do
                        lns' <- get lns
                        writeLinesToFile (head $ tail cmd') lns'
      "transform" -> transform (tail cmd') lns -- want to set lines
      "quit"      -> exitWith ExitSuccess
      _ -> return ()
  return ()

transform :: [String] -> IORef ([Line Int]) -> IO ()
transform args lns 
  | null args = return ()
  | otherwise = 
    case head args of
      "rotate" -> case (length (tail args) == 1) of
                  True -> let (_:t:_) = args in do
                            lns' <- get lns
                            lns $= fmap (applyMatrixToLine (rotationMatrix (read t :: Float))) lns'
                  _    -> return ()
      "scale" -> case (length (tail args) == 2) of
                  True -> let (a:b:_) = tail args in do
                           lns' <- get lns
                           lns $= fmap (applyMatrixToLine (scaleMatrix (read a :: Float) (read b :: Float) )) lns'
                  _    -> return ()
      "translate" -> case (length (tail args) == 2) of
                       True -> let (a:b:_) = tail args in do
                                lns' <- get lns
                                lns $= fmap (applyMatrixToLine (translateMatrix (read a :: Float) (read b :: Float) )) lns'
                       _    -> return () 
      "complex" -> case (length (tail args) == 0) of
                    True -> do
                            lns' <- get lns
                            let matrix = Just (translateMatrix (-300) (-350)) 
                                         <>> rotationMatrix 15 
                                         <>> translateMatrix 300 350
                            case matrix of
                              Just m -> lns $= fmap (applyMatrixToLine m) lns'
                              Nothing -> return ()
                    _    -> return ()
      _         -> return ()


--display lines clipped to viewport
display viewport lns = do
  vp <- get viewport
  lns' <- get lns
  glClear gl_COLOR_BUFFER_BIT
  displayViewport vp
  mapM_ (displayLine vp) lns'
  glFlush
  swapBuffers

--show a single line
displayLine viewport line = do
  let clippedLine = cohenSutherland viewport line
  case clippedLine of
         Just ln -> bresenham Nothing ln
         _ -> return ()

--show outline of viewport
displayViewport (Viewport minx miny maxx maxy) = do
  bresenham Nothing (Line (minx, miny) (minx, maxy))
  bresenham Nothing (Line (minx, maxy) (maxx, maxy))
  bresenham Nothing (Line (maxx, maxy) (maxx, miny))
  bresenham Nothing (Line (minx, miny) (maxx, miny))
  return ()
  
   
reshape :: ReshapeCallback
reshape (Size w h) = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  let wf = fromIntegral w
      hf = fromIntegral h
  glOrtho 0 wf 0 hf 0 1 --set coordinates, bottom left -> 0,0
  glMatrixMode gl_MODELVIEW

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

myInit :: IO ()
myInit = do
   glClearColor 0 0 0 0
   glTranslatef 0.375 0.375 0 -- draw to center of pixels
