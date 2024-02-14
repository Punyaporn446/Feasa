#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: Capture And Read (by Serial Number)
#
#  DESCRIPTION: This example demonstrates how to establish
#  a communication with the Feasa LED Analyser using the SN
#  instead of the COM port; then, perform a measurement and
#  download or read back the results
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
FeasaCom_Open = FeasaDLL['FeasaCom_OpenSN']
FeasaCom_Open.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_Open.restype = ctypes.c_int
FeasaCom_Close = FeasaDLL['FeasaCom_CloseSN']
FeasaCom_Close.argtypes = [ctypes.c_char_p]
FeasaCom_Close.restype = ctypes.c_int
FeasaCom_Send = FeasaDLL['FeasaCom_SendSN']
FeasaCom_Send.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_Send.restype = ctypes.c_int
FeasaCom_EnumPorts = FeasaDLL['FeasaCom_EnumPorts']
FeasaCom_EnumPorts.restype = ctypes.c_int

# A buffer is needed to store the responses obtained from the LED Analyser.
# This buffer shuld be big enought to fit all the information returned
BUFFER_SIZE = 32
buffer = ctypes.create_string_buffer(BUFFER_SIZE)

# This command enumerates the existing ports to find out
# what are the serial ports available on your computer and
# the devices connected to them. You need to execute this
# command everytime you plug or unplug a Feasa Device,
# whlie the application is running
# FeasaCom_EnumPorts()

# define port number
serial_number = b'A001'

# define fiber number
num_fib = 1

# open port
ret = FeasaCom_Open(serial_number, b"57600")
if ret == 1:
	# port opened successfully

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(serial_number, b'CAPTURE', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(serial_number)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(serial_number)
		quit()

	# shows the received data in the screen
	print (buffer.value.decode('ascii'))

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(serial_number, b'GETRGB' + b'%02d'%num_fib, buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(serial_number)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(serial_number)
		quit()

	# shows the received data in the screen
	print (buffer.value.decode('ascii'))

	# close the port
	FeasaCom_Close(serial_number)
else:
	# unable to open port
	print ('Unable to open the port')

