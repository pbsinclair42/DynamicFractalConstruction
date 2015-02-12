## Dynamic Fractal Construction

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/DisplayImage.png)

### Contents:


1. Program Description
2. Instructions for use
3. Technical Notes
4. Acknowledgements



### 1. Program Description

This program investigates what happens to different LSystem-defined fractals when you change the angle involved in their creation.  For example, the Koch Curve fractal is normally built by repeatedly replacing the middle third of each line by what would be the other 2 sides of an equilateral triangle.  In this program however you can dynamically alter the size of that angle from 60 degrees to any other size.  

Fractals included are:

1) The Koch Curve

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/1.PNG)

2) The Dragon Curve

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/2.PNG)

3) A cross

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/5.PNG)

4) A branch

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/4.PNG)

5) The Sierpinski Gasket.  

![alt tag](https://raw.githubusercontent.com/pbsinclair42/DynamicFractalConstruction/master/Screenshots/3.PNG)


### 2. Instructions for use


* Up/Down (or W/S): Increase/decrease the size of the angle.  This is done in steps of 2 degrees.  
* Left/Right (or A/D): Switch to the previous/next fractal.  There are 5 different fractals that you can investigate.  
* 1 - 5: Jump to the corresponding fractal.  This can also be used to reset the angle of the fractal you are currently on to the default angle by jumping to the same fractal.  
* F: Flip the fractal.  This effectively changes the sign of the angle.  
* Q/E: Increase/decrease the quality of the fractal. If your computer takes too long to load the fractals, use Q to reduce the quality, or if they load quickly and you'd like extra detail, use E to increase the quality.  Note that if you change the quality of the second or fifth fractal, the Dragon Curve and Sierpinski Gasket, it will appear to rotate a certain amount.  This is not a bug, this is a deliberate side effect to the way it is constructed which means that the detail of the fractal affects the angle at which it is displayed.  
* Space: Toggle boundaries.  Each fractal has a range of angles which produce a typical example of that fractal.  However, angles outwith this range still produce an output, so use this to toggle whether or not you wish to view the 'atypical' curves.  Note that the third fractal, the cross, does not have any atypical outputs.  


### 3. Technical Notes

To run this program, ensure the three .hs files are all in the same folder, then in WinGHCi, navigate to that folder, then type:

`:load "dynamicFractalConstruction.hs"`

followed by:

`main`
 
This should launch the program in a separate window.  

If your computer is running too slowly to display the fractals at higher levels of detail, press Q to lower the quality.  The processing power increases exponentially as the quality increases.  

If you experience any bugs with this program, please do let me know.  


### 4. Acknowledgements

The original LSystem.hs file was provided by the University of Edinburgh, though has since been heavily modified.  I was assisted in learning how OpenGL works by the tutorial available on [haskell.org](http://goo.gl/l80v8), and a few lines of code were taken from there and modified.  The definition of a dragon curve was taken from [cgjennings.ca](http://goo.gl/Fd0hZ8).  
