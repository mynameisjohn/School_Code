#------------------------------------------------------------
# simple_graphics.py : graphics demo using Tkinter
#------------------------------------------------------------

from Tkinter import *           # import all names from Tkinter
import math

root = Tk()                     # create top-level Tk object

da = 4*math.pi/110
a = 0.0

# define a function to draw a simulated scope trace
def draw_trace():

    global a, da

    w.delete(ALL)           # delete everything in the window

    # draw a frame around the canvas
    w.create_rectangle( 50, 50, 350, 350)

    for i in range( 100):
        x = 50 + 300.0*(i/100.0)
        y = math.sin(a) * 100 + 200
        if i > 1:
            w.create_line( x0, y0, x, y)
        x0 = x
        y0 = y
        a += da
    
    w.after( 100, draw_trace)

# create a "Canvas" widget we can draw on as a child of the root object
w = Canvas( root, width=400, height=400)
w.pack()                        # display it

draw_trace()

mainloop()                 # start the main event loop

# we never get here... the main loop just looks for events
# and handles them.

