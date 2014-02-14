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
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

import Data.Maybe(catMaybes)

data Mode = Rotate | Scale | Translate | Animate deriving (Show, Eq)

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize

   initialDisplayMode    $= [ DoubleBuffered, RGBMode ]
   initialWindowSize     $= Size 400 400
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   
   viewport <- newIORef (Viewport 0 0 400 400)
   lns3d    <- newIORef (cube :: [Line3d Double])
   --distance, screen size, width, height, user pos
   dist     <- newIORef (60  :: Double)
   ssize    <- newIORef (30  :: Double)
   width    <- newIORef (400 :: Double)
   height   <- newIORef (400 :: Double)
   --(6, 8, 7.5)
   pos      <- newIORef ((6, 8, 7.5) :: (Double, Double, Double))
   mode     <- newIORef (Translate :: Mode)
    
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard mode dist ssize width height pos lns3d viewport)
   displayCallback $= (display viewport dist ssize width height pos lns3d)
      
   idleCallback $= Just (idle mode lns3d)
   
   mainLoop
   exitWith ExitSuccess

--keyboard :: KeyboardMouseCallback
keyboard mode dist ssize width height pos lns vp (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard mode dist ssize width height pos lns vp (Char c)   Down _ _ = do
  lns' <- get lns
  m    <- get mode
  d    <- get dist
  s    <- get ssize
  lns $= performStep m c lns'
  case c of
    'R' -> mode $= Rotate
    'S' -> mode $= Scale
    'T' -> mode $= Translate
    'A' -> mode $= Animate
    'C' -> lns  $= cube
    '1' -> dist $= succ d
    '2' -> dist $= pred d
    '3' -> ssize $= succ s
    '4' -> ssize $= pred s
    'm' -> lns $= lns' ++ ((fmap (applyMatrixToLine3d (rotate3d X 90))) (take 12 lns'))
    _   -> return ()

keyboard mode dist ssize width height pos lns vp (SpecialKey c) Down _ _ = do
    w <- get width
    h <- get height
    (Viewport minx miny maxx maxy) <- get vp
    case c of
      KeyUp     -> do
        vp     $= Viewport minx miny maxx (maxy + 5)
      KeyDown   -> do
        vp     $= Viewport minx miny maxx (maxy - 5)
      KeyLeft   -> do
        vp     $= Viewport minx miny (maxx - 5) maxy
      KeyRight  -> do
        vp     $= Viewport minx miny (maxx + 5) maxy
      _         -> return ()    

keyboard _ _ _ _ _ _ _ _ _ _ _ _ = return ()

performStep m c lns = do
  case m of
    Translate ->
      let t = case c of
                'a' -> translate3d 0.1 0 0
                'd' -> translate3d (-0.1) 0 0
                'w' -> translate3d 0 0.1 0
                's' -> translate3d 0 (-0.1) 0
                'e' -> translate3d 0 0 0.1
                'q' -> translate3d 0 0 (-0.1)
                _   -> translate3d 0 0 0
      in fmap (applyMatrixToLine3d t) lns
    Scale     ->
      let t = case c of
                'a' -> scale3d (1.1) 1 1
                'd' -> scale3d (0.9) 1 1
                'w' -> scale3d 1 1.1 1
                's' -> scale3d 1 (0.9) 1
                'e' -> scale3d 1 1 1.1
                'q' -> scale3d 1 1 (0.9)
                _   -> scale3d 1 1 1
      in fmap (applyMatrixToLine3d t) lns
    Rotate    ->
      let t = case c of
                'a' -> rotate3d X 1
                'd' -> rotate3d X (-1)
                'w' -> rotate3d Y 1
                's' -> rotate3d Y (-1)
                'e' -> rotate3d Z 1
                'q' -> rotate3d Z (-1)
                _   -> rotate3d X 0
      in fmap (applyMatrixToLine3d t) lns
    Animate   -> let (Just t) = (rotate3d X 0.5) <> (rotate3d Y 0.5) <>> (rotate3d Z (-0.5))
                 in fmap (applyMatrixToLine3d t) lns

idle mode lns = do
  m <- get mode
  lns' <- get lns
  case m of
    Animate -> lns $= performStep Animate ' ' lns'
    _ -> return ()
  postRedisplay Nothing
  
--display lines clipped to viewport
display viewport dist ssize width height pt lns = do
  vp <- get viewport
  dist' <- get dist
  ssize' <- get ssize
  width' <- get width
  height' <- get height
  pt' <- get pt
  lns' <- get lns
  --origin and axes
  let   o = (0, 0, 0)
        x = (100, 0, 0)
        y = (0, 100, 0)
        z = (0, 0, 100)
        axes = zipWith Line3d (repeat o) [x, y, z]
  glClear gl_COLOR_BUFFER_BIT
  displayViewport vp
  glColor3f 1 0 0
  
  --display axes
  let [xaxis, yaxis, zaxis] = (projectLinesForRendering dist' ssize' width' height' pt' axes)
  glColor3f 1 0 0
  displayLine vp xaxis
  glColor3f 0 1 0
  displayLine vp yaxis
  glColor3f 0 0 1
  displayLine vp zaxis
  
  glColor3f 1 1 1
  mapM_ (displayLine vp) (projectLinesForRendering dist' ssize' width' height' pt' lns')
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

myInit :: IO ()
myInit = do
   glClearColor 0 0 0 0
   glTranslatef 0.375 0.375 0 -- draw to center of pixels