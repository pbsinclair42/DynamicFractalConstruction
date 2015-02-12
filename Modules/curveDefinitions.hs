--Paul Sinclair 
--s1337523
--Module that defines how each curve is constructed

module CurveDefinitions(
    pointToSize,
	theCanvas,
	draw,
    curves, cL
   )
   where

--import LSystem drawing module
import LSystem

--Curves, their default angles, and their recommended exclusion range
curves :: [(((Int -> Int -> Command),Int),(Int,Int))]
curves = [((koch,60),(90,270)), ((dragon,90),(90,270)), ((cross,90),(360,360)), ((branch,22),(120,240)),((sierpinski,60),(72,288))]

cL :: Int
cL = length curves

--Koch curve definition (as an LSystem)
koch :: Int -> Int -> Command
koch angle detail = GrabPen white :#: Turn (-90) :#: f (detail+4)
 where
 f 0 = Go 1
 f x = f(x-1) :#: p :#: f(x-1) :#: n :#: n :#: f(x-1) :#: p :#: f(x-1)
 n   = Turn (fromIntegral$(-1)*angle)
 p   = Turn (fromIntegral angle)

--Dragon curve definition (as an LSystem)
dragon :: Int -> Int -> Command
dragon angle detail = GrabPen red :#: f :#: g (detail+11)
 where
 f = Go 1
 g 0 = Sit
 g x = g(x-1) :#: p :#: h(x-1) :#: f
 h 0 = Sit
 h x = f :#: g(x-1) :#: n :#: h(x-1)
 p = Turn (fromIntegral angle)
 n = Turn (fromIntegral$(-1)*angle)

--Cross definition (as an LSystem)

cross :: Int -> Int -> Command
cross angle detail = GrabPen brown :#: f (detail + 3)
 where
 f 0 = Go 1
 f x = f(x-1) :#: g(x-1) :#: Branch (n :#: f(x-1)) :#: Branch (f(x-1)) :#: Branch (p :#: f(x-1))  :#: Branch(k :#: f(x-1))
 g 0 = Go 1
 g x = copy 3$ g(x-1)
 p = Turn (fromIntegral$(-1)*angle)
 n = Turn (fromIntegral angle)
 k = Turn 180

--Branch definition (as an LSystem)

branch :: Int -> Int -> Command
branch angle detail = GrabPen green :#: g (detail + 4)
 where
 g 0 = Go 1
 g x = f(x-1) :#: n :#: Branch(Branch(g(x-1)) :#: p :#: g(x-1)) :#: p :#: f(x-1) :#: Branch(p :#: f(x-1) :#: g(x-1)) :#: n :#: g(x-1)
 f 0 = Go 1
 f x = f(x-1) :#: f(x-1)
 n = Turn (fromIntegral angle)
 p = Turn (fromIntegral$(-1)*angle)

--Pascal's triangle definition (as an LSystem)
sierpinski :: Int -> Int -> Command
sierpinski angle detail = GrabPen blue :#: Turn (-30) :#: f (detail + 6)
 where
 f 0 = Go 1
 f x = g(x-1) :#: n :#: f(x-1) :#: n :#: g(x-1)
 g 0 = Go 1
 g x = f(x-1) :#: p :#: g(x-1) :#: p :#: f(x-1)
 n = Turn (fromIntegral angle)
 p = Turn (fromIntegral$(-1)*angle)