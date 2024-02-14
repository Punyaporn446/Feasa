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

# ------ Feasa SO Library and functions ------
FeasaLIB = cdll.LoadLibrary('libfeasacom.x86_64.so')
FeasaCom_OpenSN = FeasaLIB['FeasaCom_OpenSN']
FeasaCom_OpenSN.argtypes = [ctypes.c_char_p, ctypes.c_int]
FeasaCom_OpenSN.restype = ctypes.c_int
FeasaCom_CloseSN = FeasaLIB['FeasaCom_CloseSN']
FeasaCom_CloseSN.argtypes = [ctypes.c_char_p]
FeasaCom_CloseSN.restype = ctypes.c_int
FeasaCom_SendSN = FeasaLIB['FeasaCom_SendSN']
FeasaCom_SendSN.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
FeasaCom_SendSN.restype = ctypes.c_int
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

# set port
SN = b'76A6'

# fiber to read
num_fib = 1

# open port
ret = FeasaCom_OpenSN(SN, 57600)
if ret == 1:
	# port opened successfully

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_SendSN(SN, b'CAPTURE', buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_CloseSN(SN)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_CloseSN(SN)
		quit()

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_SendSN(SN, b'GETHSI' + b'%02d'%num_fib, buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_CloseSN(SN)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_CloseSN(SN)
		quit()

	# shows the received data
	data = buffer.value.decode('ascii').split(' ')
	print ('Hue: ' + data[0])
	print ('Saturation: ' + data[1])
	print ('Intensity: ' + data[2])

	# close the port
	FeasaCom_CloseSN(SN)
else:
	# unable to open port
	print ('Unable to open the port')


