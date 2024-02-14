#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  Display Tester examples
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
#  functions related to the Display Tester. This library is read
#  in runtime so you have to be sure that the file feasacom64.dll
#  exists in the same location of the EXE or in windows/system32
#  folder, however some compillers allow to reference the DLL
#  library from alternative locations using absolute or relative
#  paths.
#
#  Note: there are 32 and 64-bit versions of the DLL, so one or
#  the other has to be used depending on the compiler/IDE platform
#  or binary target platform.
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

# ------ Feasa DLL and functions ------
FeasaDLL = ctypes.WinDLL('feasacom64.dll')
FeasaCom_Open = FeasaDLL['FeasaCom_Open']
FeasaCom_Open.argtypes = [ctypes.c_int, ctypes.c_char_p]
FeasaCom_Open.restype = ctypes.c_int
FeasaCom_Close = FeasaDLL['FeasaCom_Close']
FeasaCom_Close.argtypes = [ctypes.c_int]
FeasaCom_Close.restype = ctypes.c_int
FeasaCom_Send = FeasaDLL['FeasaCom_Send']
FeasaCom_Send.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_Send.restype = ctypes.c_int
FeasaCom_EnumPorts = FeasaDLL['FeasaCom_EnumPorts']
FeasaCom_EnumPorts.restype = ctypes.c_int
FeasaCom_IsConnected = FeasaDLL['FeasaCom_IsConnected']
FeasaCom_IsConnected.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_IsConnected.restype = ctypes.c_int
FeasaCom_Detect = FeasaDLL['FeasaCom_Detect']
FeasaCom_Detect.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p]
FeasaCom_Detect.restype = ctypes.c_int
FeasaCom_DetectSN = FeasaDLL['FeasaCom_DetectSN']
FeasaCom_DetectSN.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_char_p]
FeasaCom_DetectSN.restype = ctypes.c_int

# A buffer is needed to store the responses obtained from the Display Tester.
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
Ports = (ctypes.c_int * 20)()

#Perform detection
nDetected = FeasaCom_Detect(Ports, b'AUTO');
if nDetected>0:
	print ('Devices detected:')
	for i in range(0, nDetected):
		print ('...on port COM' + str(Ports[i]))
else:
	print ('Unable to send the command!')

# Initialize variables
lstSerials = plstSerials = None
[lstSerials, plstSerials] = CreateCArrayOfStrings(50, 10)

#Perform detection
nDetected = FeasaCom_DetectSN(plstSerials, b'AUTO');
if nDetected>0:
	print ('Devices detected:')
	for i in range(0, nDetected):
		print ('...SN ' + lstSerials[i].value.decode('ascii'))
else:
	print ('Unable to send the command!')

# Find out if the Display Tester is connected
portNumber = FeasaCom_IsConnected(serial_number, b"57600")
if portNumber == -1:
	# shows info text
	print ('The Display Tester with SN: ' + serial_number.decode('ascii') + ' was not found')
	quit()
else:
	# port found
	print ('Display Tester found on port COM' + str(portNumber))
	
# open port
ret = FeasaCom_Open(portNumber, b"57600")
if ret == 1:
	# port opened successfully

	# Send command to the Display Tester
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(portNumber, b'GETSTATUS', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(portNumber)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(portNumber)
		quit()

	# shows info text
	print (buffer.value.decode('ascii'))

	# close the port
	FeasaCom_Close(portNumber)
else:
	# unable to open port
	print ('Unable to open the port')


