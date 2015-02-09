--Paul Sinclair 
--s1337523
--Fractal curve visualisation

--Import modified LSystem drawing + curve definitions
import CurveDefinitions
--Import graphic module
import Graphics.UI.GLUT hiding (Angle)
--Import IO functions
import Data.IORef(IORef, newIORef)

--main
main :: IO ()
main = do
  --Create the window
  initialDisplayMode    $= [DoubleBuffered]
  initialWindowSize     $= pointToSize theCanvas
  w                     <- createWindow "Investigating Angles Of LSystem Fractals"
  --Define initial values
  angle                 <- newIORef 60
  curve                 <- newIORef 0
  restrictionOn         <- newIORef True 
  detail                <- newIORef 2
  --Define callbacks
  displayCallback       $= display angle curve detail
  idleCallback          $= Just idle
  keyboardMouseCallback $= Just (keyboardMouse angle curve restrictionOn detail)
  mainLoop

--Draw the appropriate curve
display angle curve detail = do 
  a <- get angle
  c <- get curve
  d <- get detail
  draw ((fst(fst(curves!!c))) a d)

--Check if any keys have been pressed
keyboardMouse :: IORef Int -> IORef Int -> IORef Bool -> IORef Int -> KeyboardMouseCallback
keyboardMouse a c t d key Down _ _ = case key of
   --Change the angle if the relevant key is pressed
  (SpecialKey KeyUp)       -> do cInt  <- get c
                                 tBool <- get t
                                 a     $~! (\x -> if ((x+2) `mod` 360)<=fst(snd(curves!!cInt)) || ((x+2) `mod` 360)>=snd(snd(curves!!cInt)) || tBool==False then ((x+2) `mod` 360) else x)
  (Char 'w')               -> do cInt  <- get c
                                 tBool <- get t
                                 a     $~! (\x -> if ((x+2) `mod` 360)<=fst(snd(curves!!cInt)) || ((x+2) `mod` 360)>=snd(snd(curves!!cInt)) || tBool==False then ((x+2) `mod` 360) else x)
  (SpecialKey KeyDown)     -> do cInt  <- get c
                                 tBool <- get t
                                 a     $~! (\x -> if ((x+358) `mod` 360)<=fst(snd(curves!!cInt)) || ((x+358) `mod` 360)>=snd(snd(curves!!cInt)) || tBool==False then ((x+358) `mod` 360) else x)
  (Char 's')               -> do cInt  <- get c
                                 tBool <- get t
                                 a     $~! (\x -> if ((x-2) `mod` 360)<=fst(snd(curves!!cInt)) || ((x-2) `mod` 360)>=snd(snd(curves!!cInt)) || tBool==False then ((x+358) `mod` 360) else x)
  --Change the type of curve if the relevant key is pressed and set the angle to the default angle for that curve
  (SpecialKey KeyLeft)     -> do c    $~! (\x -> (x-1) `mod` cL )
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char 'a')               -> do c    $~! (\x -> (x-1) `mod` cL )
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (SpecialKey KeyRight)    -> do c    $~! (\x -> (x+1) `mod` cL )
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char 'd')               -> do c    $~! (\x -> (x+1) `mod` cL )
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char '1')               -> do c    $~! (\x -> 0)
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char '2')               -> do c    $~! (\x -> 1)
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char '3')               -> do c    $~! (\x -> 2)
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char '4')               -> do c    $~! (\x -> 3)
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  (Char '5')               -> do c    $~! (\x -> 4)
                                 cInt <- get c
                                 a    $~! (\x -> snd(fst(curves!!cInt)))
  --Flip the curve if the relevant key is pressed
  (Char 'f')               -> a $~! (\x -> 360-x)
  --Toggle restrictions
  (Char ' ')               -> do cInt  <- get c
                                 aInt  <- get a
                                 tBool <- get t
                                 t     $~! (\x -> not x)
                                 a     $~! (\x -> if tBool==False && (aInt>fst(snd(curves!!cInt)) && aInt<snd(snd(curves!!cInt))) then snd(fst(curves!!cInt)) else x)
  --Change detail if the relevant key is pressed
  (Char 'q')                  -> d $~! (\x -> if x>0 then(x-1) else x)
  (Char 'e')                  -> d $~! (\x -> if x<4 then (x+1) else x)
  --Otherwise, don't do anything
  _                        -> return ()
keyboardMouse _ _ _ _ _ _ _ _ = return ()

--Refresh the screen
idle = postRedisplay Nothing