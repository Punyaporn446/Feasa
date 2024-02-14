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

		# populate ports list
		ports = []
		for p in range(1, 255):
			ports.append(str(p))
		self.lstPorts['values'] = ports

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
		

	def createWidgets(self):
		# controls
		self.frameCOM = ttk.LabelFrame(self, text='Communications Setup', width='183', height='50')
		self.lblCOM = ttk.Label(self.frameCOM, text='COM Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameCOM, width='10', textvariable=self.lstPorts_Value, state='readonly')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='183', height='290')
		self.btnCapture = ttk.Button(self.frameCommands, text='CAPTURE', command=self.btnCapture_Click, width='20')
		self.lblFiber = ttk.Label(self.frameCommands, text='Fiber to Read:', justify='left')
		self.numFibers = tk.Spinbox(self.frameCommands, from_=5, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameCommands, text='READ DATA', command=self.btnRead_Click, width='20')
		self.frameData = ttk.LabelFrame(self, text='Data', width='220', height='350')
		self.txtLog = tk.Text(self.frameData, wrap='word', width='25', height='20', background='white', borderwidth='1')

		# position and resize
		self.frameCOM.grid_propagate(0)
		self.frameCOM.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1)
		self.lblFiber.grid(row=1, column=0, pady=3)
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=1, column=0, pady=5, padx=5)
		self.btnCapture.grid(row=0, column=0, columnspan=2, pady=15, padx=5)
		self.numFibers.grid(row=1, column=1, pady=3)
		self.btnRead.grid(row=2, column=0, columnspan=2, pady=3, padx=5)
		self.frameData.grid_propagate(0)
		self.frameData.grid(row=0, column=1, rowspan=2, pady=5, padx=5)
		self.txtLog.grid_propagate(0)
		self.txtLog.grid(padx=5)


	def btnCapture_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# while the application is running
		# self.FeasaCom_EnumPorts()

		# get selected port
		port = int(self.lstPorts.get())

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
			
			messagebox.showinfo('Done', 'Capture Successful')
			
			# close the port
			self.FeasaCom_Close(port)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnRead_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# while the application is running
		# self.FeasaCom_EnumPorts()

		# get selected port
		port = int(self.lstPorts.get())

		# get fiber number
		nfibers = int(self.numFibers.get())
		
		# open port
		ret = self.FeasaCom_Open(port, b"57600")
		if ret == 1:
			# port opened successfully

                        # retrieve measurements of all sensors/fibers
			for f in range(1, nfibers + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_Send(port, b'GETRGBI' + b'%02d'%f, buffer)
				if ret == -1:
					messagebox.showwarning('Error', 'Unable to send the command!')
					self.FeasaCom_Close(port)
					return
				elif ret == 0:
					messagebox.showwarning('Error', 'Timeout detected!')
					self.FeasaCom_Close(port)
					return

				# shows the received data in the screen
				self.txtLog.insert(tk.END, '\r\n' + '%02d'%f + ' ' + buffer.value.decode('ascii'))

			# close the port
			self.FeasaCom_Close(port)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')
		

app = Application()
app.master.title('getRGBI - (c) Feasa Enterprises Ltd')
app.mainloop()

