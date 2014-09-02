#------------------------------------------------------------
# simple_graphics.py : graphics demo using Tkinter
#------------------------------------------------------------

from Tkinter import *           # import all names from Tkinter
import math

import serial
import time

root = Tk()                     # create top-level Tk object

da = 4*math.pi/110
a = 0.0

xmin=100
ymin=100
xmax=900
ymax=900
windowWidth=1000
windowHeight=1000

ser = serial.Serial('COM3',9600,timeout=1)

# the Arduino reboots when you do this, so a delay is needed
print "Waiting for Arduino to reset..."
time.sleep( 3.0)

# define a function to draw a simulated scope trace
def draw_trace():

    ser.write( 't')

    global a, da

    w.delete(ALL)           # delete everything in the window

    # draw a frame around the canvas
    w.create_rectangle( xmin, ymin, xmax, ymax)

    for i in range( 100):
        
        x = xmin + (xmax-xmin)*(i/100.0)
        s = ser.readline()
        y = ymax - int(s)*((ymax-ymin)/1023.0)
        
        #math.sin(a) * 100 + 200
        if i > 1:
            w.create_line( x0, y0, x, y)
        x0 = x
        y0 = y
    
    w.after( 100, draw_trace)

# create a "Canvas" widget we can draw on as a child of the root object
w = Canvas( root, width=windowWidth, height=windowHeight)
w.pack()                        # display it

draw_trace()

mainloop()                 # start the main event loop

# we never get here... the main loop just looks for events
# and handles them.

