import Data.Bits

type Point = (Int, Int)
data Line = Line {p_x :: Point, p_y :: Point} deriving (Show, Eq)
data Viewport = Viewport {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving (Show, Eq)

inside = 0; -- 0000
left = 1;   -- 0001
right = 2;  -- 0010
bottom = 4; -- 0100
top = 8;    -- 1000

mkViewport :: Int -> Int -> Int -> Int -> Maybe Viewport
mkViewport minx miny maxx maxy 
  | minx < maxx && miny < maxy = Just (Viewport minx miny maxx maxy)
  | otherwise = Nothing

-- Produces a line clipped across the viewport if one exists. If it does not intersect, returns Nothing
cohenSutherland :: Viewport -> Line -> Maybe Line
cohenSutherland v@(Viewport minx miny maxx maxy) l@(Line p1@(x1, y1) p2@(x2, y2)) =
  if not (bitTest (oc1 .|. oc2)) then Just l  -- completely inside viewport
  else if bitTest (oc1 .&. oc2)  then Nothing -- completely outside viewport
  else cohenSutherland v (newLine clipPoint)  -- partially inside viewport
  where [oc1, oc2] = fmap (getOutCode v) [p1, p2]
        outcode    = max oc1 oc2 --one is 0; choose the non-zero one
        newLine p  = if outcode == oc1 then Line p p2 else Line p1 p
        bitTest x = x /= 0
        -- compute intersection point
        clipPoint 
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
        getOutCode :: Viewport -> Point -> Int
        getOutCode (Viewport minx miny maxx maxy) (x, y) = (handleX . handleY) inside
          where handleX code | x < minx = code .|. left --left
                             | x > maxx = code .|. right --right
                             | otherwise = code
                handleY code | y < miny = code .|. bottom --bottom
                             | y > maxy = code .|. top --top
                             | otherwise = code

testViewPort = Viewport 10 10 60 60
testLine1 = Line (11, 11) (59, 59)
testLine2 = Line (0,0) (0, 100)
testLine3 = Line (0, 0) (70, 70)