#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: Search & Detect
#
#  DESCRIPTION: This example demonstrates how to list
#  all available Feasa devices, and also to locate
#  the port number of a connected Device based on its serial
#  number.
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

def CreateCArrayOfStrings(ArraySize, ptringLength):
            #Function extracted from FeasaCom.py - (c) Feasa Enterprises Ltd 2019
	Variable = [ctypes.create_string_buffer(ptringLength) for i in range(ArraySize)]
	Pointer = (ctypes.c_char_p*ArraySize)(*map(ctypes.addressof, Variable))
	for i in range(ArraySize):
		Variable[i].value = b""
	return [Variable, Pointer]

# ------ Feasa SO Library and functions ------
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
FeasaCom_IsConnected = FeasaLIB['FeasaCom_IsConnected']
FeasaCom_IsConnected.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int]
FeasaCom_IsConnected.restype = ctypes.c_int
FeasaCom_Detect = FeasaLIB['FeasaCom_Detect']
FeasaCom_Detect.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
FeasaCom_Detect.restype = ctypes.c_int
FeasaCom_DetectSN = FeasaLIB['FeasaCom_DetectSN']
FeasaCom_DetectSN.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
FeasaCom_DetectSN.restype = ctypes.c_int

# A buffer is needed to store the responses obtained from the LED Analyser.
# This buffer shuld be big enought to fit all the information quit()ed
BUFFER_SIZE = 1024
buffer = ctypes.create_string_buffer(BUFFER_SIZE)

# get Serial Number
serial_number = b"92AC"

# This command enumerates the existing ports to find out
# what are the serial ports available on your computer and
# the devices connected to them. You need to execute this
# command everytime you plug or unplug a Feasa Device,
# while the application is running
FeasaCom_EnumPorts()

# Initialize variables
DevPaths = pDevPaths = None
[DevPaths, pDevPaths] = CreateCArrayOfStrings(50, 255)

#Perform detection
nDetected = FeasaCom_Detect(pDevPaths, 0);
if nDetected>0:
	print ('Devices detected:')
	for i in range(0, nDetected):
		print ('...on port ' + DevPaths[i].value.decode('ascii'))
else:
	print ('Unable to send the command!')

# Initialize variables
lstSerials = plstSerials = None
[lstSerials, plstSerials] = CreateCArrayOfStrings(50, 10)

#Perform detection
nDetected = FeasaCom_DetectSN(plstSerials, 0);
if nDetected>0:
	print ('Devices detected:')
	for i in range(0, nDetected):
		print ('...SN ' + lstSerials[i].value.decode('ascii'))
else:
	print ('Unable to send the command!')


# Find out if the LED Analyser is connected
DevPath = ''
r = FeasaCom_IsConnected(buffer, serial_number, 57600)
if r != 1:
	# shows info text
	print ('The LED Analyser with SN: ' + serial_number.decode('ascii') + ' was not found')
	quit()
else:
	# port found
	DevPath = buffer.value
	print ('LED Analyser found on port ' + str(DevPath))

# open port
ret = FeasaCom_Open(DevPath, 57600)
if ret == 1:
	# port opened successfully

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(DevPath, b'GETSTATUS', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(DevPath)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(DevPath)
		quit()

	# shows info text
	print (buffer.value.decode('ascii'))

	# close the port
	FeasaCom_Close(DevPath)
else:
	# unable to open port
	print ('Unable to open the port')


