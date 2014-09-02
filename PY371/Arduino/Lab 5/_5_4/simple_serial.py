#
# simple python communications demo
# use with Lab 5.4 sketch

import serial
import time

# connect to the Arduino via the serial port
# change the serial port name as required (to i.e. 'COM6' if on windows)
ser = serial.Serial( 'COM3', 9600, timeout=1)

# the Arduino reboots when you do this, so a delay is needed
print "Waiting for Arduino to reset..."
time.sleep( 3.0)

print "Sending trigger..."
ser.write( 't')
for i in range( 100):
    s = ser.readline()
    print s

