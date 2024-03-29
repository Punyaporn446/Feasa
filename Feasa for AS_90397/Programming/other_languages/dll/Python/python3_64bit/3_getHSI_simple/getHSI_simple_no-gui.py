#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: getHSI
#
#  DESCRIPTION: This example demonstrates how to perform a
#  capture from the Feasa LED Analyser and then retrieve the
#  Hue, Saturation and Intensity values from a given
#  Fiber/sensor number, extracting the numerical values from
#  the string received.
#
#  This example uses a dynamic library to access to the
#  functions related to the LED Analyser. This library is read
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
#**************************************************************

import ctypes
from ctypes import *

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

# set port
port = 15

# fiber to read
num_fib = 1

# open port
ret = FeasaCom_Open(port, b"57600")
if ret == 1:
	# port opened successfully

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(port, b'CAPTURE', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(port)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(port)
		quit()

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(port, b'GETHSI' + b'%02d'%num_fib, buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(port)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(port)
		quit()

	# shows the received data
	data = buffer.value.decode('ascii').split(' ')
	print ('Hue: ' + data[0])
	print ('Saturation: ' + data[1])
	print ('Intensity: ' + data[2])

	# close the port
	FeasaCom_Close(port)
else:
	# unable to open port
	print ('Unable to open the port')


