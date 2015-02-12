--Paul Sinclair 
--s1337523
--Module that defines how to display an LSystem

module LSystem (
    pointToSize,
	theCanvas,
	draw,
    Command (..),
	copy,
    Pen (..), black, white, red, brown, green, blue,
   )
    where

--Importing graphic modules
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef(IORef, newIORef)

-- Points
data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

-- Colours
data Pen = Colour GL.GLfloat GL.GLfloat GL.GLfloat
         | Inkless
           deriving (Eq, Ord, Show)

penToRGB :: Pen -> GL.Color3 GL.GLfloat
penToRGB (Colour r g b)  =  GL.Color3 r g b
penToRGB Inkless  =  error "penToRGB: inkless"

white, black :: Pen
white = Colour 1.0 1.0 1.0
black = Colour 0.0 0.0 0.0
red   = Colour 1.0 0.0 0.0
brown = Colour 0.66 0.43 0.26
green = Colour 0.28 0.89 0.196
blue  = Colour 0.0 0.0 1.0

-- Lines
data Ln = Ln Pen Pnt Pnt
  deriving (Eq,Ord,Show)

-- Window parameters
theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = penToRGB black

-- Drawing LSystem commands
draw :: Command -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background 
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pen startP endP)  =
    GL.color (penToRGB pen) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3 
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)

-- Turtle commands (Turtles turn counter-clockwise and start facing up)
type Angle    = Float
type Distance = Float
type Turtle   = (Pen,Angle,Pnt)

data Command = Go Distance
             | Turn Angle 
             | Command :#: Command
			 | Sit
			 | Branch Command
             | GrabPen Pen
               deriving (Eq, Ord, Show)

infixr 5 :#:

--Copy a turtle command
copy :: Int -> Command -> Command
copy a x = foldr1 (:#:) $ replicate a x

-- Converting commands to GL graphics
execute :: Command -> [Ln]
execute c  =  lines
  where
  (lines, turtle)  =  f c (black, 0, Pnt 0 0)

  f :: Command -> Turtle -> ([Ln], Turtle)
  f (c :#: d) turtle             =  (clines ++ dlines, dturtle)
                                    where
                                    (clines, cturtle) = f c turtle
                                    (dlines, dturtle) = f d cturtle
  f (Branch c) turtle            =  (clines, turtle)
                                    where
                                    (clines, cturtle) = f c turtle
  f (Go dst) (pen,ang,pnt)       =  (if pen == Inkless
                                       then []
                                       else [Ln pen pnt endpnt],
                                     (pen,ang,endpnt))
                                    where
                                    endpnt = pnt + scalar dst * polar ang
  f (Turn delta) (pen,ang,pnt)   =  ([], (pen,ang-delta,pnt))
  f (GrabPen new) (old,ang,pnt)  =  ([], (new,ang,pnt))
  f (Sit) turtle		 =  ([], turtle)
  
-- Rescales all points in a list of lines from an arbitrary scale to (-1.-1) - (1.1)
rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pen p q)  =  Ln pen (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pen p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360