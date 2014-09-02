#
# fancier oscilloscope with a few controls
#

from Tkinter import *
import serial
import random
import time

width = 800                     # window width
nlines = 800                    # window height

vmargin = 25                    # vertical margins eventually for labels, etc

# scale for ADC values 0-1023 to fit in vertical region
vscale = float(nlines-(vmargin*2))/1024

# sampling rate options
sample_labels = ["1ms", "500us", "200us", "100us", "50us", "20us", "10us" ];
sample_us     = [1000, 500, 200, 100, 50, 20, 10 ]

root = Tk()                     # create top-level Tk object

# trigger mode selector variable
tmode = IntVar()

# define a function to
# trigger the scope and display a waveform
def do_trigger():

    # set the sampling rate from the listbox
    sel = lbox.curselection()    # get selection from listbox
    my_us = 100                 # default rate if not found
    # selection is a tuple (list)
    if len(sel):                 # if there was a selection
        # this is a bit odd because listbox.curselection() returns
        # a tuple of strings (why?)
        my_us = sample_us[ int(sel[0]) ] # and convert to us

    ser.write( "s");            # send to Arduino
    ser.write( str(my_us) );
    ser.write( "\n");
    c = ser.read( size=1)
    if c != ">":                # should send back '>'
        print "Comms error"

    ser.write( "t")         # send the trigger command
    ser.write( "\n");
    s = ser.readline()      # read first line with number of points

    np = int(s)
    data = []               # create an empty list to store dat

    for i in range( np):    # loop to read the data points
        s = ser.readline()
        v = int(s)
        data.append( v)     # add to the list
    y = data[0]             # first point

    w.delete(ALL)      # delete everything in the window
    # draw top and bottom margin lines
    w.create_line( 0, vmargin, width-1, vmargin)
    w.create_line( 0, nlines-vmargin, width-1, nlines-vmargin)

    hscale = width/np       # calculate horizontal scale

    for i in range(1,np):   # loop to plot the data points
        w.create_line( hscale*(i-1), nlines-vmargin-vscale*y,
                            hscale*i, nlines-vmargin-vscale*data[i])
        y = data[i]

    # if in AUTO mode, re-trigger after a delay
    if tmode.get() == 0:
        w.after( 100, do_trigger)

    c = ser.read( size=1)
    if c != ">":                # should send back '>'
        print "Comms error"

#--- end of do_trigger() function ---


#-------------------- main program --------------------

# connect to the Arduino via the serial port
ser = serial.Serial( 'COM3', 9600, timeout=1)

# the Arduino reboots when you do this, so a delay is needed
time.sleep( 3.0)

# flush the input buffer
while ser.inWaiting():
    c = ser.read( size=1)

# create a frame to hold everything
frame = Frame(root)
frame.pack()

#--- create some buttons and controls
# use the grid() layout manager to place them in a table
# all the controls so far go in row 0

# QUIT button
Button( frame, text="QUIT", fg="red", command=frame.quit).grid(row=0, column=0)

# SINGLE trigger button
Button( frame, text="SINGLE", fg="darkgreen", command=do_trigger).grid(row=0, column=1)

# trigger mode radio buttons
Radiobutton( frame, text="Single", variable=tmode, value=2).grid( row=0, column=2)
Radiobutton( frame, text="Normal", variable=tmode, value=1).grid( row=0, column=3)
Radiobutton( frame, text="Auto", variable=tmode, value=0).grid( row=0, column=4)

# sampling interval list box
Label( frame, text="Period ").grid( row=0, column=5)
lbox = Listbox( frame)
lbox.grid( row=0, column=6)
# add labels to the list box
for item in sample_labels:
    lbox.insert( END, item)

# create a "Canvas" object we can draw on as a child of the top
w = Canvas( frame, width=800, height=800)
# place on the next row of the grid, spanning all columns
w.grid( columnspan=8)

# trigger once to get things going
do_trigger()

root.mainloop()                 # start the main event loop
