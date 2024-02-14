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
FeasaCom_ListPortsDetected = FeasaDLL['FeasaCom_ListPortsDetected']
FeasaCom_ListPortsDetected.argtypes = [ctypes.POINTER(ctypes.c_int)]
FeasaCom_ListPortsDetected.restype = ctypes.c_int
FeasaTools = ctypes.WinDLL('feasala_tools64.dll')
Feasa_Parse_RGBI = FeasaTools['Feasa_Parse_RGBI']
Feasa_Parse_RGBI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), POINTER(ctypes.c_int)]
Feasa_Parse_RGBI.restype = ctypes.c_int
Feasa_Parse_HSI = FeasaTools['Feasa_Parse_HSI']
Feasa_Parse_HSI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Feasa_Parse_HSI.restype = ctypes.c_int

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

# port
port = 15

# fiber number
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
	Hue = (ctypes.c_float)()
	Saturation = (ctypes.c_int)()
	Intensity = (ctypes.c_int)()
	ret = Feasa_Parse_HSI(buffer.value, Hue, Saturation, Intensity)
	print ('Hue: ' + str(Hue.value))
	print ('Saturation: ' + str(Saturation.value))
	print ('Intensity: ' + str(Intensity.value))

	# Send command to the LED Analyser
	# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
	ret = FeasaCom_Send(port, b'GETRGBI' + b'%02d'%num_fib, buffer)
	if ret == -1:
		print ('Unable to send the command!')
		FeasaCom_Close(port)
		quit()
	elif ret == 0:
		print ('Timeout detected!')
		FeasaCom_Close(port)
		quit()

	# shows the received data
	Red = (ctypes.c_ubyte)()
	Green = (ctypes.c_ubyte)()
	Blue = (ctypes.c_ubyte)()
	ret = Feasa_Parse_RGBI(buffer.value, Red, Green, Blue, Intensity)
	print ('Red: ' + str(Red.value))
	print ('Green: ' + str(Green.value))
	print ('Blue: ' + str(Blue.value))

	# close the port
	FeasaCom_Close(port)
else:
	# unable to open port
	print ('Unable to open the port')

