#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: DaisyChain (Bus capture)
#
#  DESCRIPTION: This example demonstrates how to perform
#  a capture for all Daisy-chained analysers, through the
#  SO Library functions and then retrieve the HSI values for
#  the fiber requested.
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

from tkinter import ttk
import tkinter as tk
import ctypes
from ctypes import *
from tkinter import messagebox

class Application(ttk.Frame):

	def __init__(self, master=None):
                #variables
		self.lstPorts_Value = 1
		self.lstCaptureMode_Value = "AUTO"
		self.lstCaptureRange_Value = "MEDIUM"
		self.lstCaptureFrame_Value = 5

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()

		# add widgets to window
		self.createWidgets()

		# ------ Feasa SO Library and functions ------
		self.FeasaLIB = cdll.LoadLibrary('libfeasacom.x86_64.so')
		self.FeasaCom_Open = self.FeasaLIB['FeasaCom_Open']
		self.FeasaCom_Open.argtypes = [ctypes.c_char_p, ctypes.c_int]
		self.FeasaCom_Open.restype = ctypes.c_int
		self.FeasaCom_Close = self.FeasaLIB['FeasaCom_Close']
		self.FeasaCom_Close.argtypes = [ctypes.c_char_p]
		self.FeasaCom_Close.restype = ctypes.c_int
		self.FeasaCom_Send = self.FeasaLIB['FeasaCom_Send']
		self.FeasaCom_Send.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
		self.FeasaCom_Send.restype = ctypes.c_int
		self.FeasaCom_EnumPorts = self.FeasaLIB['FeasaCom_EnumPorts']
		self.FeasaCom_EnumPorts.restype = ctypes.c_int
		self.FeasaCom_IsPortAvailable = self.FeasaLIB['FeasaCom_IsPortAvailable']
		self.FeasaCom_IsPortAvailable.argtypes = [ctypes.c_char_p]
		self.FeasaCom_IsPortAvailable.restype = ctypes.c_int
		self.FeasaCom_DaisyChain_Add = self.FeasaLIB['FeasaCom_DaisyChain_Add']
		self.FeasaCom_DaisyChain_Add.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
		self.FeasaCom_DaisyChain_Add.restype = ctypes.c_int
		self.FeasaCom_DaisyChain_Capture = self.FeasaLIB['FeasaCom_DaisyChain_Capture']
		self.FeasaCom_DaisyChain_Capture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_DaisyChain_Capture.restype = ctypes.c_int
		

                # set default serial
		self.txtSN.insert(0,"E218")
		
		# populate ports list
		ports = []
		for p in range(0, 20):
			if self.FeasaCom_IsPortAvailable(('/dev/' + 'ttyS' + str(p)).encode('ascii')) == 1:
				ports.append('ttyS' + str(p))
		for p in range(0, 10):
			if self.FeasaCom_IsPortAvailable(('/dev/' + 'ttyUSB' + str(p)).encode('ascii')) == 1:
				ports.append('ttyUSB' + str(p))
		self.lstPorts['values'] = ports
		if len(ports) > 0:
			self.lstPorts.set(ports[0])

		# populate list of Capture Modes
		self.lstCaptureMode_Options = ["AUTO", "MANUAL"]
		self.lstCaptureMode['values'] = self.lstCaptureMode_Options
		self.lstCaptureMode.set("AUTO")

		# populate list of Capture Ranges
		self.lstCaptureRange_Options = ["LOW", "MEDIUM", "HIGH", "SUPER", "ULTRA"]
		self.lstCaptureRange['values'] = self.lstCaptureRange_Options
		self.lstCaptureRange.set("MEDIUM")
		

	def createWidgets(self):
		# controls
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='223', height='335')
		self.lblCOM = ttk.Label(self.frameSetup, text='Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='6', textvariable=self.lstPorts_Value, state='readonly')
		self.lblSelect = ttk.Label(self.frameSetup, text='Select the port of the 1st Analyser (Bus Master) and then add all the devices attached to the Bus in the list below.', foreground='red', wraplength='160', justify='left')
		self.lblDeviceList = ttk.Label(self.frameSetup, text='Daisy Chained Analysers:', justify='left')
		self.btnAdd = ttk.Button(self.frameSetup, text='ADD Analyser:', command=self.btnAdd_Click, width='15')
		self.txtSN = ttk.Entry(self.frameSetup, text='ADD Analyser:', width='6')
		self.lstAnalysers = tk.Listbox(self.frameSetup, width='25', height='6')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='280', height='190')
		self.lblCaptureMode = ttk.Label(self.frameCommands, text='Capture Mode: ', justify='left')
		self.lstCaptureMode = ttk.Combobox(self.frameCommands, width='29', textvariable=self.lstCaptureMode_Value, state='readonly')
		self.lblCaptureRange = ttk.Label(self.frameCommands, text='Range (manual): ', justify='left')
		self.lstCaptureRange = ttk.Combobox(self.frameCommands, width='10', textvariable=self.lstCaptureRange_Value, state='readonly')
		self.lblFiber = ttk.Label(self.frameCommands, text='Fiber to Read:', justify='left')
		self.numFiber = tk.Spinbox(self.frameCommands, from_=1, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameCommands, text='BUS CAPTURE + READ', command=self.btnRead_Click, width='30')
		self.frameResponse = ttk.LabelFrame(self, text='Response', width='280', height='140')
		self.txtLog = tk.Text(self.frameResponse, wrap='word', width='32', height='6', background='white', borderwidth='1')

		# position and resize
		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, rowspan=3, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1, sticky='E')
		self.lblSelect.grid(row=1, column=0, columnspan=2, padx=5, pady=5)
		self.lblDeviceList.grid(row=2, column=0, columnspan=2, padx=5, pady=5, sticky='W')
		self.btnAdd.grid(row=3, column=0, padx=5, sticky='W')
		self.txtSN.grid(row=3, column=1, padx=5, sticky='W')
		self.lstAnalysers.grid(row=4, column=0, columnspan=2, padx=5, pady=5, sticky='W')
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=0, column=1, pady=5, padx=5)
		self.lblCaptureMode.grid(row=0, column=0, pady=3, sticky='E')
		self.lstCaptureMode.grid(row=1, column=0, columnspan=2, pady=3)
		self.lblCaptureRange.grid(row=2, column=0, pady=3, sticky='E')
		self.lstCaptureRange.grid(row=2, column=1, pady=3)
		self.lblFiber.grid(row=5, column=0, pady=3, sticky='E')
		self.numFiber.grid(row=5, column=1, pady=3)
		self.btnRead.grid(row=6, column=0, columnspan=2, pady=3, padx=5)
		self.frameResponse.grid_propagate(0)
		self.frameResponse.grid(row=1, column=1, padx=5)
		self.txtLog.grid(pady=3, padx=5)


	def btnAdd_Click(self):
		# get Serial Number
		serial_number = self.txtSN.get()
		
		# validate SN
		if len(serial_number) != 4:
			messagebox.showwarning('Error', 'Bad Serial Number')
			return

		# add serial number to list
		self.lstAnalysers.insert(0,serial_number)

		# Clear results box
		self.txtSN.delete(0, tk.END)


	def btnRead_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# check if Any Analyser has been added to the bus
		if self.lstAnalysers.size() < 1:
			messagebox.showinfo('Add Analysers', 'Please, add the Analysers attached to the BUS first')
			return

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# once your application is running
		# self.FeasaCom_EnumPorts()

		# get selected port
		DevPath = ('/dev/' + self.lstPorts.get()).encode('ascii')

		# get fiber number
		num_fib = int(self.numFiber.get())

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			# Add Analysers to the Daisy-Chain bus
			for i in range(0, self.lstAnalysers.size()):
				ret = self.FeasaCom_DaisyChain_Add(DevPath, self.lstAnalysers.get(i).encode())

			# Get Capture Command
			CaptureRange = 0
			i = self.lstCaptureMode_Options.index(self.lstCaptureMode.get())
			if i==0:
				CaptureRange = 0
			elif i==1:
				CaptureRange = self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1

			# Perform a Bus Capture
			self.txtLog.insert(tk.END, '\r\nCapturing...')
			ret = self.FeasaCom_DaisyChain_Capture(DevPath, 0, CaptureRange, 0)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return
			self.txtLog.insert(tk.END, ' Done!')

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b'BUSFREE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			# Get serial number of Bus Master/Main device
			ret = self.FeasaCom_Send(DevPath, b'GETSERIAL', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return
			sn_main = buffer.value.decode('ascii')

			# Get measurements from Bus Master/Main device
			ret = self.FeasaCom_Send(DevPath, b'GETHSI' + b'%02d'%num_fib, buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			# shows the received data in the screen
			self.txtLog.insert(tk.END, '\r\nFib ' + '%02d'%num_fib + '(' + sn_main + '): ' + buffer.value.decode('ascii'))
			
			# Get measurements from all the Devices
			for i in range(0, self.lstAnalysers.size()):
				# Get the bus for the device to read
				ret = self.FeasaCom_Send(DevPath, b'BUSGET' + self.lstAnalysers.get(i).encode(), buffer)
				if ret == -1:
					messagebox.showwarning('Error', 'Unable to send the command!')
					self.FeasaCom_Close(DevPath)
					return
				elif ret == 0:
					messagebox.showwarning('Error', 'Timeout detected!')
					self.FeasaCom_Close(DevPath)
					return

				# Get measurement
				ret = self.FeasaCom_Send(DevPath, b'GETHSI' + ('%02d'%num_fib).encode(), buffer)
				if ret == -1:
					messagebox.showwarning('Error', 'Unable to send the command!')
					self.FeasaCom_Close(DevPath)
					return
				elif ret == 0:
					messagebox.showwarning('Error', 'Timeout detected!')
					self.FeasaCom_Close(DevPath)
					return

				# shows the received data in the screen
				self.txtLog.insert(tk.END, '\r\nFib ' + '%02d'%num_fib + '(' + self.lstAnalysers.get(i) + '): ' + buffer.value.decode('ascii'))

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b'BUSFREE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

app = Application()
app.master.title('Daisy Chain Bus Capture - (c) Feasa Enterprises Ltd')
app.mainloop()

