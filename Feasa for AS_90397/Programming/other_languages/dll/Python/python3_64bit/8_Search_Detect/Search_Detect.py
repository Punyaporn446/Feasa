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
		self.FeasaCom_IsConnected = self.FeasaDLL['FeasaCom_IsConnected']
		self.FeasaCom_IsConnected.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
		self.FeasaCom_IsConnected.restype = ctypes.c_int
		self.FeasaCom_Detect = self.FeasaDLL['FeasaCom_Detect']
		self.FeasaCom_Detect.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p]
		self.FeasaCom_Detect.restype = ctypes.c_int
		self.FeasaCom_DetectSN = self.FeasaDLL['FeasaCom_DetectSN']
		self.FeasaCom_DetectSN.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_char_p]
		self.FeasaCom_DetectSN.restype = ctypes.c_int
		

	def createWidgets(self):
		# controls
		self.frameInfo = ttk.LabelFrame(self, text='Info', width='170', height='345')
		self.lblInfo = ttk.Label(self.frameInfo, text='In this example we will show how to find a LED Analyser by its Serial Number and then open a communication to retrieve its status.\nHere will be also shown how to detect and list all Analysers connected.', wraplength='150', justify='left')
		self.btnDetect = ttk.Button(self.frameInfo, text='Detect Ports', command=self.btnDetect_Click, width='20')
		self.btnDetectSerials = ttk.Button(self.frameInfo, text='Detect Serials', command=self.btnDetectSerials_Click, width='20')
		self.frameSearch = ttk.LabelFrame(self, text='Search', width='230', height='100')
		self.lblSN = ttk.Label(self.frameSearch, text='Serial Number to find:', justify='left')
		self.txtSN = ttk.Entry(self.frameSearch, width='6')
		self.btnSearch = ttk.Button(self.frameSearch, text='FIND LED ANALYSER &\n SHOW THE STATUS', command=self.btnSearch_Click, width='34')
		self.frameStatus = ttk.LabelFrame(self, text='Status', width='230', height='240')
		self.txtLog = tk.Text(self.frameStatus, width='30', height='14', background='white', borderwidth='1', font=('Courier','9'))

		# position and resize
		self.frameInfo.grid_propagate(0)
		self.frameInfo.grid(row=0, column=0, rowspan=2, pady=5, padx=5)
		self.lblInfo.grid(row=0, column=0, padx=5)
		self.btnDetect.grid(row=1, column=0, padx=5, pady=3)
		self.btnDetectSerials.grid(row=2, column=0, padx=5, pady=3)
		self.frameSearch.grid_propagate(0)
		self.frameSearch.grid(row=0, column=1, pady=5, padx=5)
		self.lblSN.grid(row=0, column=0, padx=5, pady=5)
		self.txtSN.grid(row=0, column=1, padx=5, pady=3, sticky='W')
		self.btnSearch.grid(row=1, column=0, columnspan=2, padx=5, pady=3)
		self.frameStatus.grid_propagate(0)
		self.frameStatus.grid(row=1, column=1, pady=2, padx=5)
		self.txtLog.grid(pady=3, padx=5)

	def CreateCArrayOfStrings(self, ArraySize, ptringLength):
		#Function extracted from FeasaCom.py - (c) Feasa Enterprises Ltd 2019
		Variable = [ctypes.create_string_buffer(ptringLength) for i in range(ArraySize)]
		Pointer = (ctypes.c_char_p*ArraySize)(*map(ctypes.addressof, Variable))
		for i in range(ArraySize):
			Variable[i].value = b""
		return [Variable, Pointer]


	def btnDetect_Click(self):
		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# whlie the application is running
		self.FeasaCom_EnumPorts()

		# Initialize variables
		Ports = (ctypes.c_int * 20)()

		#Perform detection
		nDetected = self.FeasaCom_Detect(Ports, b'AUTO');
		if nDetected>0:
			self.txtLog.insert(tk.END, 'Devices detected:\r\n')
			for i in range(0, nDetected):
				self.txtLog.insert(tk.END, '...on port COM' + str(Ports[i]) + '\r\n')
		else:
			messagebox.showwarning('Error', 'Unable to send the command!')


	def btnDetectSerials_Click(self):
		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# whlie the application is running
		self.FeasaCom_EnumPorts()

		# Initialize variables
		lstSerials = plstSerials = None
		[lstSerials, plstSerials] = self.CreateCArrayOfStrings(50, 10)

		#Perform detection
		nDetected = self.FeasaCom_DetectSN(plstSerials, b'AUTO');
		if nDetected>0:
			self.txtLog.insert(tk.END, 'Devices detected:\r\n')
			for i in range(0, nDetected):
				self.txtLog.insert(tk.END, '...SN ' + lstSerials[i].value.decode('ascii') + '\r\n')
		else:
			messagebox.showwarning('Error', 'Unable to send the command!')

	def btnSearch_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# get Serial Number
		serial_number = self.txtSN.get()
		
		# validate SN
		if len(serial_number) != 4:
			messagebox.showwarning('Error', 'Bad Serial Number')
			return

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# whlie the application is running
		# self.FeasaCom_EnumPorts()

		# Find out if the LED Analyser is connected
		portNumber = self.FeasaCom_IsConnected(serial_number.encode(), b"57600")
		if portNumber == -1:
			# shows info text
			self.txtLog.insert(tk.END, 'The LED Analyser with SN: ' + serial_number + ' was not found')
			return
		else:
			# port found
			self.txtLog.insert(tk.END, 'LED Analyser found on port ' + str(portNumber) + '\r\n')

		# open port
		ret = self.FeasaCom_Open(portNumber, b"57600")
		if ret == 1:
			# port opened successfully

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(portNumber, b'GETSTATUS', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(portNumber)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(portNumber)
				return

			# shows info text
			self.txtLog.insert(tk.END, buffer.value.decode('ascii'))

			# close the port
			self.FeasaCom_Close(portNumber)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')


app = Application()
app.master.title('Search LED Analyser - (c) Feasa Enterprises Ltd')
app.mainloop()

