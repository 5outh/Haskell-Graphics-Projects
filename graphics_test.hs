{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar ( ($=), ($=!), get )

import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window

import Graphics.Rendering.OpenGL.Raw.Core
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

import Data.List (sort,unfoldr)
import System.Random
import Control.Monad
import System.Exit
import Data.List.Split
import Data.Maybe(fromJust, listToMaybe, catMaybes)
import Data.IORef
import System.Environment

type IntPoint = Point Int
type GLPoint = Point GLfloat

type Point a = (a, a)
data Line a  = Line (Point a) (Point a)

myInit :: IO ()
myInit = do
   glClearColor 0 0 0 0
   glTranslatef 0.375 0.375 0 -- draw to center of pixels

--display :: Some IORefs -> DisplayCallback
display numLines sfn fn stripe = do
   n <- get numLines
   sn <- get sfn
   fn <- get fn
   stripe' <- get stripe
   glClear gl_COLOR_BUFFER_BIT
   replicateM n (sn (fn stripe'))
   glFlush
   swapBuffers

displayLine line stripe alg = do
  line' <- get line 
  stripe' <- get stripe
  alg' <- get alg
  glClear gl_COLOR_BUFFER_BIT
  alg' stripe' line' --actually draws everything
  glFlush
  swapBuffers

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

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize

   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 640 640
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   
   numLines <- newIORef 0 --lines to draw
   sfn <- newIORef randomLongLine --long or short lines
   alg <- newIORef (bresenham :: Maybe Int -> Line Int -> IO ()) -- line drawing algorithm
   stripe <- newIORef (Nothing :: Maybe Int) -- stripe lines, default to no
   line <- newIORef $ Line (0,0) (0,0)
   
   --set stripe when running program first time
   case _args of
    [_, "-s", n] -> stripe $= Just (read n :: Int)
    ["-s", n]    -> stripe $= Just (read n :: Int)
    _            -> stripe $= Nothing
    
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= let (a:rgs) = _args in 
      if a == "-line" then (displayLine line stripe alg)
      else (display numLines sfn alg stripe)
      
   idleCallback $= let (a:rgs) = _args in 
     if a == "-line" then Just (idleLine line alg)
     else Just (idleRand numLines sfn alg stripe)
   
   mainLoop


idleRand numLines sfn alg stripe = do
  processInput numLines sfn alg stripe
  postRedisplay Nothing

idleLine line alg = do
  processInputForLine line alg
  postRedisplay Nothing

{- 
<algorithm> <line_length> <# of lines>
-}

--processInput :: Very ugly type signature.
processInput numLines sfn alg stripe = do
  cmd <- getLine
  let cmd' = splitOn " " cmd
      [c1, c2, c3] | length cmd' >= 3 = take 3 cmd'
                   | otherwise = take 3 $ cmd' ++ repeat " "
      n = maybeRead c3 :: (Maybe Int)
      lineMappings = [("short", randomShortLine), ("long", randomLongLine)]
      algMappings = [("bresenham", bresenham), ("std", lineFromTo)]
      f = lookup c1 lineMappings
      g = lookup c2 algMappings
  numLines `assignMaybe` n
  sfn `assignMaybe` f
  alg `assignMaybe` g
  
  case (head cmd') of
    "exit" -> do
      putStrLn "Goodbye!"
      exitWith ExitSuccess
    _      -> return ()

{- <algorithm> x1 y1 x2 y2 -}
processInputForLine line alg = do
  cmd <- getLine
  ln <- get line
  let cmd' = splitOn " " cmd
      cmds@[alg', x1, y1, x2, y2] | length cmd' >= 5 = take 5 cmd'
                                 | otherwise = take 5 $ cmd' ++ repeat " "
      pts@[x1', y1', x2', y2'] = catMaybes $ fmap (\x -> maybeRead x :: Maybe Int) (tail cmds)
      newLine | (length pts) == 4 = Just $ Line (x1', y1') (x2', y2')
              | otherwise = Nothing
      algMappings = [("bresenham", bresenham), ("std", lineFromTo)]
      newAlg = lookup alg' algMappings
      
  line `assignMaybe` newLine
  alg `assignMaybe` newAlg
  
  case (head cmd') of
    "exit" -> do
      putStrLn "Goodbye!"
      exitWith ExitSuccess
    _      -> return ()

assignMaybe :: IORef a -> Maybe a -> IO ()
assignMaybe a (Just b) = a $= b
assignMaybe a Nothing  = return ()

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

--draw a line from point a to point b using the basic algorithm
lineFromTo :: Maybe Int -> Line Int -> IO ()
lineFromTo stripe (Line a@(x1, y1) b@(x2, y2)) =
    let dx = abs $ x2-x1
        dy = abs $ y2-y1
	  in if dx >= dy && x2 < x1 then lineFromTo' stripe (Line b a)
	     else if dy >= dx && y2 < y1 then lineFromTo' stripe (Line b a)
	     else lineFromTo' stripe (Line a b)

lineFromTo' :: Maybe Int -> Line Int -> IO ()
lineFromTo' stripe (Line (x1, y1) (x2, y2)) = do
  glBegin gl_POINTS
  if ( dy < dx ) then
    forM_ [x1..x2] $ \x -> do
      glColor3f 1 1 1
      let skip = case stripe of 
                      (Just s) -> x+1 `mod` s == 0 || x `mod` s == 0
                      _        -> False
      if (not skip) then 
        (uncurry glVertex2f) (xFunc x)
      else return ()
  else
    forM_ [y1..y2] $ \y -> do
      glColor3f 1 1 1
      let skip = case stripe of 
                      (Just s) -> y+1 `mod` s == 0 || y `mod` s == 0
                      _        -> False
      if (not skip) then
        (uncurry glVertex2f) (yFunc y)
      else return ()
  glEnd
  where dx = abs $ x2 - x1
        dy = abs $ y2 - y1
        xFunc x = toGLPoint (x, y1 + (dy * (x - x1)) `safeDiv` dx)
        yFunc y = toGLPoint (x1 + (dx * (y - y1)) `safeDiv` dy, y)
        pts | dy < dx   = fmap xFunc [x1..x2]
		        | otherwise = fmap yFunc [y1..y2]
        safeDiv x y | y == 0 = 0
                    | otherwise = x `div` y

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

toGLPoint :: IntPoint -> GLPoint
toGLPoint (a, b) = (fromIntegral a, fromIntegral b)

swapCoords :: (a, b) -> (b, a)
swapCoords (a, b) = (b, a)

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

{----------
TESTING
----------}

bresenhamTest :: Maybe Int -> Line Int -> IO [GLPoint]
bresenhamTest stripe (Line a@(x0, y0) b@(x1, y1)) = do
   let xs = catMaybes $ unfoldr go (err, x0', y0')
   return xs
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
                    
--draw a line from point a to point b using the basic algorithm
lineFromToTest :: Maybe Int -> Line Int -> IO ()
lineFromToTest stripe (Line a@(x1, y1) b@(x2, y2)) = 
    let dx = abs $ x2-x1
        dy = abs $ y2-y1
	  in if dx >= dy && x2 < x1 then lineFromToTest' stripe (Line b a)
	     else if dy >= dx && y2 < y1 then lineFromToTest' stripe (Line b a)
	     else lineFromToTest' stripe (Line a b)

lineFromToTest' :: Maybe Int -> Line Int -> IO ()
lineFromToTest' stripe (Line (x1, y1) (x2, y2)) = do
  if ( dy < dx ) then
    forM_ [x1..x2] $ \x -> do
      let skip = case stripe of 
                      (Just s) -> x+1 `mod` s == 0 || x `mod` s == 0
                      _        -> False
      if (not skip) then 
        return ()
      else return ()
  else
    forM_ [y1..y2] $ \y -> do
      let skip = case stripe of 
                      (Just s) -> y+1 `mod` s == 0 || y `mod` s == 0
                      _        -> False
      if (not skip) then
        return ()
      else return ()
  where dx = abs $ x2 - x1
        dy = abs $ y2 - y1
        xFunc x = toGLPoint (x, y1 + (dy * (x - x1)) `safeDiv` dx)
        yFunc y = toGLPoint (x1 + (dx * (y - y1)) `safeDiv` dy, y)
        pts | dy < dx   = fmap xFunc [x1..x2]
		        | otherwise = fmap yFunc [y1..y2]
        safeDiv x y | y == 0 = 0
                    | otherwise = x `div` y

shortLinesB n = replicateM_ n $ randomShortLine (bresenhamTest Nothing)   
longLinesB n = replicateM_ n $ randomLongLine (bresenhamTest Nothing)   
shortLinesS n = replicateM_ n $ randomShortLine (lineFromToTest Nothing)   
longLinesS n = replicateM_ n $ randomLongLine (lineFromToTest Nothing)                     