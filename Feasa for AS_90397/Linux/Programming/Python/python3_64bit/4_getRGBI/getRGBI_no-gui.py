#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: getRGBI
#
#  DESCRIPTION: This example demonstrates how to perform a
#  capture from the Feasa LED Analyser and retrieve the RGBI
#  color data for all the fibers, extracting each one of the
#  data values from the string received and displaying them
#  on a formatted table
#
#  This example uses a dynamic library to access to the
#  functions related to the LED Analyser. This library is read
#  in runtime, so you have to be sure that the file
#  libfeasacom_x86_64.so has been copied to the /usr/lib/
#  directory or equivalent, moreover,  some compillers/IDE
#  allow to reference the SO library from the same location
#  of the binary/script or alternative locations using absolute
#  or relative paths.
#
#  Note: there are 32 and 64-bit versions of the SO Library, so
#  one or the other has to be used depending on the compiler/IDE
#  platform or binary target platform.
#
#***************************************************************

import ctypes
from ctypes import *

# ------ Feasa Library and functions ------
FeasaLIB = cdll.LoadLibrary('libfeasacom.x86_64.so')
FeasaCom_Open = FeasaLIB['FeasaCom_Open']
FeasaCom_Open.argtypes = [ctypes.c_char_p, ctypes.c_int]
FeasaCom_Open.restype = ctypes.c_int
FeasaCom_Close = FeasaLIB['FeasaCom_Close']
FeasaCom_Close.argtypes = [ctypes.c_char_p]
FeasaCom_Close.restype = ctypes.c_int
FeasaCom_Send = FeasaLIB['FeasaCom_Send']
FeasaCom_Send.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_Send.restype = ctypes.c_int
FeasaCom_EnumPorts = FeasaLIB['FeasaCom_EnumPorts']
FeasaCom_EnumPorts.restype = ctypes.c_int

# A buffer is needed to store the responses obtained from the LED Analyser.
# This buffer shuld be big enought to fit all the information quit()ed
BUFFER_SIZE = 32
buffer = ctypes.create_string_buffer(BUFFER_SIZE)

# This command enumerates the existing ports to find out
# what are the serial ports available on your computer and
# the devices connected to them. You need to execute this
# command everytime you plug or unplug a Feasa Device,
# while the application is running
# FeasaCom_EnumPorts()

# DevPath port
DevPath = b'/dev/ttyUSB0'

# open port
ret = FeasaCom_Open(DevPath, 57600)
if ret == 1:
	# port opened successfully

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(DevPath, b'CAPTURE', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(DevPath)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(DevPath)
		quit()
			
	print ('Capture Successful!')
			

	# number of fibers
	nfibers = 20
		

	# retrieve measurements of all sensors/fibers
	for f in range(1, nfibers + 1):
		# Send command to the LED Analyser
		# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
		ret = FeasaCom_Send(DevPath, b'GETRGBI' + b'%02d'%f, buffer)
		if ret == -1:
			print ('Unable to send the command!')
			FeasaCom_Close(DevPath)
			quit()
		elif ret == 0:
			print ('Timeout detected!')
			FeasaCom_Close(DevPath)
			quit()

		# shows the received data in the screen
		print ('%02d'%f + ' ' + buffer.value.decode('ascii'))

	# close the port
	FeasaCom_Close(DevPath)
else:
	# unable to open port
	print ('Unable to open the port')
		

