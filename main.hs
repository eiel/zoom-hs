import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Window

main =
  do (progName, _) <- getArgsAndInitialize
     initialDisplayMode $= [RGBAMode, DoubleBuffered]
     initialWindowSize $= Size 500 500
     window <- createWindow "Window"
     cullFace $= Just Back
     rot <- newIORef $ MyRect 0.2 0.2 1 1 0
     displayCallback $= display rot
     keyboardMouseCallback $= Just (mouse window rot)
     mainLoop

data MyRect = MyRect { x :: GLfloat, y :: GLfloat, w :: GLfloat, h :: GLfloat, r :: GLfloat } deriving Show

renderRect :: MyRect -> IO ()
renderRect (MyRect x y w h r) = renderPrimitive LineLoop $ mapM_ vertex
                [helper x y ,
                 helper x2 y2,
                 helper (x2 - h * sint) (y2 + h * cost),
                 helper (x - h * sint) (y + h * cost) :: Vertex2 GLfloat ]
  where
    helper x1 y1 = Vertex2 x1 y1
    cost = cos theta
    sint = sin theta
    x2 = x + w * cost
    y2 = y + w * sint
    theta = r * pi / 180

mouse :: Window -> IORef MyRect -> Key -> KeyState -> Modifiers -> Position -> IO ()
mouse window rot key state mod pos = do
  keyEvent rot key state mod pos
  postRedisplay $ Just window

rotationMyrect :: GLfloat -> MyRect -> MyRect
rotationMyrect n (MyRect x y w h r) = MyRect x' y' w h (r+n)
  where
    theta = ((pi / 180)*)
    cost = cos . theta
    sint = sin . theta
    costr = cost (r+n)
    sintr = sint (r+n)
    (dw, dh) = result2 (theta r) w h
    cx = x + dw / 2
    cy = y + dh / 2
    px = (-w/2)
    py = (-h/2)
    x' = (px * costr) - (py * sintr) + cx
    y' = (py * costr) + (px * sintr) + cy

delta_theta dx dy = atan2 dy dx

result theta dx dy = (abs pw, abs ph)
   where
      d_theta = delta_theta dx dy
      sum_theta = d_theta - theta
      pw = cos sum_theta * xy
      ph = sin sum_theta * xy
      xy = dx / (cos d_theta)

result2 theta dx dy = (pw ,ph)
   where
      d_theta = delta_theta dx dy
      pw = cos (theta + d_theta) * xy
      ph = sin (theta + d_theta) * xy
      xy = dx / (cos d_theta)

mouseTrans :: Position -> (GLfloat, GLfloat)
mouseTrans (Position mouseX mouseY) = (x, y)
  where
    x = (fromIntegral mouseX) / 250
    y = (fromIntegral mouseY) / 250

angle r =  r / 180 * pi

centerRect :: MyRect -> (GLfloat, GLfloat)
centerRect (MyRect x y w h r) = centerPoint r (w,h) (x,y)

centerPoint :: GLfloat -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
centerPoint r (w,h) (x,y) = (x', y')
  where
    (bx, by) = rotatePoint r $ (-w/2, -h/2)
    x' = x -bx
    y' = y -by

rotatePoint :: GLfloat -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
rotatePoint r (x, y) = (x', y')
  where
    theta = angle r
    x' = x * cos theta - y * sin theta
    y' = y * cos theta + x * sin theta

movePoint :: (GLfloat, GLfloat) ->  (GLfloat, GLfloat) -> (GLfloat, GLfloat)
movePoint (mx, my) (x, y)  = (x+mx, y+my)

minusPoint :: (GLfloat, GLfloat) -> (GLfloat, GLfloat)
minusPoint (x, y) = (-x, -y)

rotationCenterPoint :: (GLfloat, GLfloat) -> GLfloat -> (GLfloat, GLfloat) -> (GLfloat, GLfloat)
rotationCenterPoint center r =  movePoint center . rotatePoint r . movePoint (minusPoint center)

calcWidthHeight mouse target r = (cx', cy')
  where
    (mouseX, mouseY) = mouse
    (tx, ty) = target
    cx = (mouseX + tx) / 2
    cy = (mouseY + ty) / 2
    rotate = rotatePoint (-r) . movePoint (-cx, -cy)
    (sx, sy) = rotate $ mouse
    (ex, ey) = rotate $ target
    cx' = abs (ex - sx)
    cy' = abs (ey - sy)

zoomBase :: (GLfloat, GLfloat) -> (GLfloat,GLfloat) -> MyRect -> MyRect
zoomBase transe@(transX, transY) mouse@(mx,my) rect@(MyRect x y w h r) =
             MyRect lx ly lw lh r
  where
    theta = angle r
    center = centerRect rect
    transPoint (xx, yy) = (xx*transX, yy*transY)
    (x',y') = movePoint (minusPoint center) $ (x,y)
    -- 動かさない基準点の算出
    (tx', ty') = rotatePoint r . transPoint $ (w/2, h/2)
    (tx, ty) = movePoint center (tx', ty')
    nc = ((mx+tx) /2, (my+ty) /2)
    (nw, nh) = calcWidthHeight mouse (tx, ty) r
    (nx', ny') = rotatePoint r $ (-nw/2, -nh/2)
    (nx, ny) = movePoint nc $ (nx', ny')
    lx = if transX == 0 then x else nx
    ly = if transY == 0 then y else ny
    lw = if transX == 0 then w else (max nw 0.1)
    lh = if transY == 0 then h else (max nh 0.1)

zoomLeftTop :: (GLfloat, GLfloat) -> MyRect -> MyRect
zoomLeftTop = zoomBase (1,1)

zoomRightTop :: (GLfloat,GLfloat) -> MyRect -> MyRect
zoomRightTop = zoomBase (-1,1)

zoomRightBottom :: (GLfloat,GLfloat) -> MyRect -> MyRect
zoomRightBottom = zoomBase (-1, -1)

zoomLeftBottom :: (GLfloat,GLfloat) -> MyRect -> MyRect
zoomLeftBottom = zoomBase (1,-1)

zoomUp = zoomBase (0,1)
zoomDown = zoomBase (0,-1)
zoomRight = zoomBase (-1,0)
zoomLeft = zoomBase (1,0)

keyEvent rot (Char 'e') _ _ _ = do n <- readIORef rot
                                   modifyIORef rot $ rotationMyrect 15
keyEvent rot (Char 'o') _ _ _ = do n <- readIORef rot
                                   modifyIORef rot $ rotationMyrect (-15)
keyEvent rot key Down _ pos =
    do n <- readIORef rot
       modifyIORef rot $ zoom $ mouseTrans $ pos
  where
    zoom = case key of
        (Char 'p') -> zoomRightTop
        (Char '\'') -> zoomLeftTop
        (Char ';') -> zoomLeftBottom
        (Char 'k') -> zoomRightBottom
        (Char 'u') -> zoomRight
        (Char '.') -> zoomUp
        (Char 'a') -> zoomLeft
        (Char 'j') -> zoomDown
        _ -> (\_ r -> r)
keyEvent rot key state mod position = do n <- readIORef rot
                                         putStrLn $ show key ++ show position
                                         putStrLn $ show n

display rot =
  do clear [ColorBuffer]
     n <- readIORef rot
     loadIdentity
     -- perspectivpe 30.0 1 1.0 100.0
     lookAt (Vertex3 1 1 0) (Vertex3 1 1 0.1) (Vector3 0 (-1) 0)
     renderRect n
     swapBuffers

