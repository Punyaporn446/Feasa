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
#  instead of the Device Path; then, perform a measurement and
#  download or read back the results
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

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()

		# add widgets to window
		self.createWidgets()

		# ------ Feasa SO Library and functions ------
		self.FeasaLIB = cdll.LoadLibrary('libfeasacom.x86_64.so')
		self.FeasaCom_OpenSN = self.FeasaLIB['FeasaCom_OpenSN']
		self.FeasaCom_OpenSN.argtypes = [ctypes.c_char_p, ctypes.c_int]
		self.FeasaCom_OpenSN.restype = ctypes.c_int
		self.FeasaCom_CloseSN = self.FeasaLIB['FeasaCom_CloseSN']
		self.FeasaCom_CloseSN.argtypes = [ctypes.c_char_p]
		self.FeasaCom_CloseSN.restype = ctypes.c_int
		self.FeasaCom_SendSN = self.FeasaLIB['FeasaCom_SendSN']
		self.FeasaCom_SendSN.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
		self.FeasaCom_SendSN.restype = ctypes.c_int
		self.FeasaCom_EnumPorts = self.FeasaLIB['FeasaCom_EnumPorts']
		self.FeasaCom_EnumPorts.restype = ctypes.c_int
		

	def createWidgets(self):
		# controls
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='183', height='295')
		self.lblSN = ttk.Label(self.frameSetup, text='Serial Number:', justify='left')
		self.txtSN = ttk.Entry(self.frameSetup, width='5')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='174', height='295')
		self.lblPress = ttk.Label(self.frameCommands, text='Press the button shown below to retrieve the Serial Number of the LED Analyser Connected to the selected port', wraplength='160', justify='left')
		self.lblFiber = ttk.Label(self.frameCommands, text='Fiber to Read:', justify='left')
		self.numFiber = tk.Spinbox(self.frameCommands, from_=1, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameCommands, text='CAPTURE AND READ', command=self.btnRead_Click, width='24')
		self.lblResults = ttk.Label(self.frameCommands, text='Results:', justify='left')
		self.txtLog = tk.Text(self.frameCommands, wrap='word', width='20', height='5', background='white', borderwidth='1')

		# position and resize
		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, pady=5, padx=5)
		self.lblSN.grid(row=0, column=0, padx=5)
		self.txtSN.grid(row=0, column=1)
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=0, column=1, pady=5, padx=5)
		self.lblPress.grid(row=0, column=0, columnspan=2, padx=5, pady=5)
		self.lblFiber.grid(row=1, column=0, pady=3)
		self.numFiber.grid(row=1, column=1, pady=3)
		self.btnRead.grid(row=2, column=0, columnspan=2, pady=10)
		self.lblResults.grid(row=3, column=0, columnspan=2, padx=5, pady=3, sticky='W')
		self.txtLog.grid_propagate(0)
		self.txtLog.grid(row=4, column=0, columnspan=2, padx=4, pady=3)


	def btnRead_Click(self):

		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# This command enumerates the existing ports to find out
		# what are the serial ports available on your computer and
		# the devices connected to them. You need to execute this
		# command everytime you plug or unplug a Feasa Device,
		# while the application is running
		# self.FeasaCom_EnumPorts()

		# get Serial Number
		serial_number = self.txtSN.get().encode()
		
		# validate SN
		if len(serial_number) != 4:
			messagebox.showwarning('Error', 'Serial number should be a 4-character text')
			return

		# get fiber number
		num_fib = int(self.numFiber.get())

		# open port
		ret = self.FeasaCom_OpenSN(serial_number, 57600)
		if ret == 1:
			# port opened successfully

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_SendSN(serial_number, b'CAPTURE', buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_CloseSN(serial_number)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_CloseSN(serial_number)
				return

			# shows the received data in the screen
			self.txtLog.insert(tk.END, buffer.value.decode('ascii'))

			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_SendSN(serial_number, b'GETRGBI' + b'%02d'%num_fib, buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_CloseSN(serial_number)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_CloseSN(serial_number)
				return

			# shows the received data in the screen
			self.txtLog.insert(tk.END, '\r\n' + buffer.value.decode('ascii'))

			# close the port
			self.FeasaCom_CloseSN(serial_number)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')
		

app = Application()
app.master.title('Capture & Read by SN - (c) Feasa Enterprises Ltd')
app.mainloop()

