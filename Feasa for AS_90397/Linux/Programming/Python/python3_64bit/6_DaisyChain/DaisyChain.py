#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: DaisyChain
#
#  DESCRIPTION: This example demonstrates how to establish
#  a communication with a daisy chained Feasa LED Analyser
#  using commands GETBUS and FREEBUS.
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
		# variables
		self.lstPorts_Value = 1

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
		

	def createWidgets(self):
		# controls
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='183', height='340')
		self.lblCOM = ttk.Label(self.frameSetup, text='Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='10', textvariable=self.lstPorts_Value, state='readonly')
		self.lblSelect = ttk.Label(self.frameSetup, text='First, you need to select the communications port where the 1st LED Analyser of the BUS is connected. This will be the port through which you can access to the bus', wraplength='160', justify='left')
		self.frameDC = ttk.LabelFrame(self, text='Daisy Chain', width='250', height='100')
		self.lblSN = ttk.Label(self.frameDC, text='Serial Number of the LED Analyser you want to control:', wraplength='240', justify='left')
		self.txtSN = ttk.Entry(self.frameDC, width='6')
		self.frameMeasure = ttk.LabelFrame(self, text='Measure', width='250', height='90')
		self.lblFiber = ttk.Label(self.frameMeasure, text='Fiber to Read:', justify='left')
		self.numFiber = tk.Spinbox(self.frameMeasure, from_=1, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameMeasure, text='CAPTURE AND READ', command=self.btnRead_Click, width='24')
		self.frameResponse = ttk.LabelFrame(self, text='Response', width='250', height='140')
		self.txtLog = tk.Text(self.frameResponse, wrap='word', width='27', height='6', background='white', borderwidth='1')

		# position and resize
		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, rowspan=3, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1)
		self.lblSelect.grid(row=1, column=0, columnspan=2, padx=5, pady=5)
		self.frameDC.grid_propagate(0)
		self.frameDC.grid(row=0, column=1, pady=5, padx=5)
		self.lblSN.grid(row=0, column=0, columnspan=2, padx=5, pady=5)
		self.txtSN.grid(row=1, column=0, columnspan=2, padx=5, pady=3, sticky='W')
		self.frameMeasure.grid_propagate(0)
		self.frameMeasure.grid(row=1, column=1, pady=2, padx=5)
		self.lblFiber.grid(row=0, column=0, pady=3)
		self.numFiber.grid(row=0, column=1, pady=3)
		self.btnRead.grid(row=1, column=0, columnspan=2, padx=5, pady=8)
		self.frameResponse.grid_propagate(0)
		self.frameResponse.grid(row=2, column=1, pady=2, padx=5)
		self.txtLog.grid(pady=3, padx=5)


	

	def btnRead_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

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

			# Assign bus to given SN
			ret = self.FeasaCom_Send(DevPath, b'BUSGET' + serial_number.encode(), buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			self.txtLog.insert(tk.END, 'The Bus belongs to the LED Analyser with SN: ' + serial_number)

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b'CAPTURE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			# shows the received data in the screen
			self.txtLog.insert(tk.END, '\r\n' + buffer.value.decode('ascii'))

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b'GETRGBI' + b'%02d'%num_fib, buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			# shows the received data in the screen
			self.txtLog.insert(tk.END, '\r\n' + buffer.value.decode('ascii'))

			# Release the bus
			ret = self.FeasaCom_Send(DevPath, b'BUSFREE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return

			self.txtLog.insert(tk.END, '\r\nThe Bus is Free!')

			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

app = Application()
app.master.title('Daisy Chain - (c) Feasa Enterprises Ltd')
app.mainloop()

