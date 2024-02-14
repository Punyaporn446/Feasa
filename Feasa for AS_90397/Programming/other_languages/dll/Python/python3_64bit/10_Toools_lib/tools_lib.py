#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: tools_lib
#
#  DESCRIPTION: This example demonstrates how to use the
#  library feasa_tools64.dll or its 32-bit equivalent
#  feasa_tools.dll, to extract or parse the strings returned
#  by the Feasa LED Analyser responses and convert them
#  into usable numerical values.
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
#***************************************************************

from tkinter import ttk
import tkinter as tk
import ctypes
from tkinter import messagebox

class Application(ttk.Frame):

	def __init__(self, master=None):
		# variables
		self.lstPorts_Value = 1

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()

		# add widgets to window
		self.createWidgets()

		# ------ Feasa DLL and functions ------
		self.FeasaDLL = ctypes.WinDLL('feasacom64.dll')
		self.FeasaCom_Open = self.FeasaDLL['FeasaCom_Open']
		self.FeasaCom_Open.argtypes = [ctypes.c_int, ctypes.c_char_p]
		self.FeasaCom_Open.restype = ctypes.c_int
		self.FeasaCom_Close = self.FeasaDLL['FeasaCom_Close']
		self.FeasaCom_Close.argtypes = [ctypes.c_int]
		self.FeasaCom_Close.restype = ctypes.c_int
		self.FeasaCom_Send = self.FeasaDLL['FeasaCom_Send']
		self.FeasaCom_Send.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_char_p]
		self.FeasaCom_Send.restype = ctypes.c_int
		self.FeasaCom_EnumPorts = self.FeasaDLL['FeasaCom_EnumPorts']
		self.FeasaCom_EnumPorts.restype = ctypes.c_int
		self.FeasaCom_ListPortsDetected = self.FeasaDLL['FeasaCom_ListPortsDetected']
		self.FeasaCom_ListPortsDetected.argtypes = [ctypes.POINTER(ctypes.c_int)]
		self.FeasaCom_ListPortsDetected.restype = ctypes.c_int
		
		self.FeasaTools = ctypes.WinDLL('feasa_tools64.dll')
		self.Feasa_Parse_RGBI = self.FeasaTools['Feasa_Parse_RGBI']
		self.Feasa_Parse_RGBI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_byte), ctypes.POINTER(ctypes.c_byte), ctypes.POINTER(ctypes.c_byte), ctypes.POINTER(ctypes.c_int)]
		self.Feasa_Parse_RGBI.restype = ctypes.c_int
		self.Feasa_Parse_HSI = self.FeasaTools['Feasa_Parse_HSI']
		self.Feasa_Parse_HSI.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
		self.Feasa_Parse_HSI.restype = ctypes.c_int

		# populate ports list
		ports = (ctypes.c_int*255)()
		nports = self.FeasaCom_ListPortsDetected(ports)
		lports = []
		if nports > 0:
			for i in range(0, nports):
				lports.append(ports[i])
		self.lstPorts['values'] = lports
		if nports > 0:
			self.lstPorts.set(ports[0])
		

	def createWidgets(self):
		# controls
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='183', height='330')
		self.lblCOM = ttk.Label(self.frameSetup, text='COM Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='10', textvariable=self.lstPorts_Value, state='readonly')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='174', height='330')
		self.lblPress = ttk.Label(self.frameCommands, text='Press the button shown below to execute a capture and read the result.', wraplength='160', justify='left')
		self.lblFiber = ttk.Label(self.frameCommands, text='Fiber to Read:', justify='left')
		self.numFiber = tk.Spinbox(self.frameCommands, from_=1, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameCommands, text='CAPTURE AND READ', command=self.btnRead_Click, width='24')
		self.lblHue = ttk.Label(self.frameCommands, text='Hue:', justify='right')
		self.lblHueRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')
		self.lblSat = ttk.Label(self.frameCommands, text='Saturation:', justify='right')
		self.lblSatRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')
		self.lblRed = ttk.Label(self.frameCommands, text='Red:', justify='right')
		self.lblRedRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')
		self.lblGreen = ttk.Label(self.frameCommands, text='Green:', justify='right')
		self.lblGreenRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')
		self.lblBlue = ttk.Label(self.frameCommands, text='Blue:', justify='right')
		self.lblBlueRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')
		self.lblInt = ttk.Label(self.frameCommands, text='Intensity:', justify='right')
		self.lblIntRes = ttk.Label(self.frameCommands, text='', justify='right', width='8', background='white', borderwidth='1')

		# position and resize
		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1)
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=0, column=1, pady=5, padx=5)
		self.lblPress.grid(row=0, column=0, columnspan=2, padx=5, pady=5)
		self.lblFiber.grid(row=1, column=0, pady=3)
		self.numFiber.grid(row=1, column=1, pady=3)
		self.btnRead.grid(row=2, column=0, columnspan=2, padx=5, pady=15)
		self.lblHue.grid(row=3, column=0, pady=3, sticky='E')
		self.lblHueRes.grid(row=3, column=1, pady=3)
		self.lblSat.grid(row=4, column=0, pady=3, sticky='E')
		self.lblSatRes.grid(row=4, column=1, pady=3)
		self.lblRed.grid(row=5, column=0, pady=3, sticky='E')
		self.lblRedRes.grid(row=5, column=1, pady=3)
		self.lblGreen.grid(row=6, column=0, pady=3, sticky='E')
		self.lblGreenRes.grid(row=6, column=1, pady=3)
		self.lblBlue.grid(row=7, column=0, pady=3, sticky='E')
		self.lblBlueRes.grid(row=7, column=1, pady=3)
		self.lblInt.grid(row=8, column=0, pady=3, sticky='E')
		self.lblIntRes.grid(row=8, column=1, pady=3)


	def btnRead_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# Clear results
		self.lblHueRes['text'] = ''
		self.lblSatRes['text'] = ''
		self.lblRedRes['text'] = ''
		self.lblGreenRes['text'] = ''
		self.lblBlueRes['text'] = ''
		self.lblIntRes['text'] = ''

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# while the application is running
		# self.FeasaCom_EnumPorts()

		# get selected port
		port = int(self.lstPorts.get())

		# get fiber number
		num_fib = int(self.numFiber.get())

		# open port
		ret = self.FeasaCom_Open(port, b"57600")
		if ret == 1:
			# port opened successfully

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(port, b'CAPTURE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(port)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(port)
				return

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(port, b'GETHSI' + b'%02d'%num_fib, buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(port)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(port)
				return

			# shows the received data
			Hue = (ctypes.c_float)()
			Saturation = (ctypes.c_int)()
			Intensity = (ctypes.c_int)()
			ret = self.Feasa_Parse_HSI(buffer.value, Hue, Saturation, Intensity)
			self.lblHueRes['text'] = Hue.value
			self.lblSatRes['text'] = Saturation.value
			self.lblIntRes['text'] = Intensity.value

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(port, b'GETRGBI' + b'%02d'%num_fib, buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(port)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(port)
				return

			# shows the received data
			Red = (ctypes.c_byte)()
			Green = (ctypes.c_byte)()
			Blue = (ctypes.c_byte)()
			ret = self.Feasa_Parse_RGBI(buffer.value, Red, Green, Blue, Intensity)
			self.lblRedRes['text'] = Red.value
			self.lblGreenRes['text'] = Green.value
			self.lblBlueRes['text'] = Blue.value

			# close the port
			self.FeasaCom_Close(port)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')
		

app = Application()
app.master.title('Tools  Library - (c) Feasa Enterprises Ltd')
app.mainloop()

