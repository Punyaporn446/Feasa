#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: capture_modes
#
#  DESCRIPTION: This example demonstrates the different methods
#  available for performing measurements (captures) on the
#  LED Analyser. Then, the responses received are parsed and
#  the numerical values are exatracted and printed to a
#  grid-style output.
#  This example uses a helper function to format the received
#  decimal string to the default Local Decimal character.
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
		self.FeasaCom_SetResponseTimeout = self.FeasaLIB['FeasaCom_SetResponseTimeout']
		self.FeasaCom_SetResponseTimeout.argtypes = [ctypes.c_uint]
		self.FeasaCom_SetResponseTimeout.restype = ctypes.c_int
		self.FeasaCom_Capture = self.FeasaLIB['FeasaCom_Capture']
		self.FeasaCom_Capture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_Capture.restype = ctypes.c_int
		
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
		self.lstCaptureMode_Options = ["AUTO", "MANUAL", "PWM: AUTO-RANGE & AUTO-FRAMING", "PWM: MANUAL-RANGE & AUTO-FRAMING", "PWM: AUTO-RANGE & MANUAL-FRAMING", "PWM: MANUAL-RANGE & MANUAL-FRAMING"]
		self.lstCaptureMode['values'] = self.lstCaptureMode_Options
		self.lstCaptureMode.set("AUTO")

		# populate list of Capture Ranges
		self.lstCaptureRange_Options = ["LOW", "MEDIUM", "HIGH", "SUPER", "ULTRA"]
		self.lstCaptureRange['values'] = self.lstCaptureRange_Options
		self.lstCaptureRange.set("MEDIUM")
		
		# populate frames list
		lframes = []
		for p in range(1, 16):
			lframes.append(str(p))
		self.lstCaptureFrame['values'] = lframes
		self.lstCaptureFrame.set(5)

	def createWidgets(self):
		# controls
		self.frameCOM = ttk.LabelFrame(self, text='Communications Setup', width='220', height='50')
		self.lblCOM = ttk.Label(self.frameCOM, text='Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameCOM, width='10', textvariable=self.lstPorts_Value, state='readonly')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='220', height='290')
		self.lblCaptureMode = ttk.Label(self.frameCommands, text='Capture Mode: ', justify='left')
		self.lstCaptureMode = ttk.Combobox(self.frameCommands, width='31', textvariable=self.lstCaptureMode_Value, state='readonly')
		self.lblCaptureRange = ttk.Label(self.frameCommands, text='Range (manual/PWM): ', justify='left')
		self.lstCaptureRange = ttk.Combobox(self.frameCommands, width='10', textvariable=self.lstCaptureRange_Value, state='readonly')
		self.lblCaptureFrame = ttk.Label(self.frameCommands, text='Frames (PWM): ', justify='left')
		self.lstCaptureFrame = ttk.Combobox(self.frameCommands, width='10', textvariable=self.lstCaptureFrame_Value, state='readonly')
		self.btnCapture = ttk.Button(self.frameCommands, text='CAPTURE', command=self.btnCapture_Click, width='23')
		self.btnCaptureEasy = ttk.Button(self.frameCommands, text='CAPTURE (easy method)', command=self.btnCaptureEasy_Click, width='23')
		self.lblFibers = ttk.Label(self.frameCommands, text='Fibers to Read:', justify='left')
		self.numFibers = tk.Spinbox(self.frameCommands, from_=5, to=20, increment=1, width='3')
		self.btnRead = ttk.Button(self.frameCommands, text='READ DATA', command=self.btnRead_Click, width='20')
		self.frameData = ttk.LabelFrame(self, text='Data', width='220', height='350')
		self.txtLog = tk.Text(self.frameData, wrap='word', width='25', height='20', background='white', borderwidth='1')

		# position and resize
		self.frameCOM.grid_propagate(0)
		self.frameCOM.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1)
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=1, column=0, pady=5, padx=5)
		self.lblCaptureMode.grid(row=0, column=0, pady=3, sticky='E')
		self.lstCaptureMode.grid(row=1, column=0, columnspan=2, pady=3)
		self.lblCaptureRange.grid(row=2, column=0, pady=3, sticky='E')
		self.lstCaptureRange.grid(row=2, column=1, pady=3)
		self.lblCaptureFrame.grid(row=3, column=0, pady=3, sticky='E')
		self.lstCaptureFrame.grid(row=3, column=1, pady=3)
		self.btnCapture.grid(row=4, column=0, columnspan=2, pady=8, padx=5)
		self.btnCaptureEasy.grid(row=5, column=0, columnspan=2, pady=8, padx=5)
		self.lblFibers.grid(row=6, column=0, pady=3, sticky='E')
		self.numFibers.grid(row=6, column=1, pady=3)
		self.btnRead.grid(row=7, column=0, columnspan=2, pady=3, padx=5)
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

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# get selected port
		DevPath = ('/dev/' + self.lstPorts.get()).encode('ascii')

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			# Compose Capcure Command
			CaptureCommand = ""
			i = self.lstCaptureMode_Options.index(self.lstCaptureMode.get())
			if i==0:
				CaptureCommand = "CAPTURE"
			elif i==1:
				CaptureCommand = "CAPTURE" + str(self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1)
			elif i==2:
				CaptureCommand = "CAPTUREPWM"
			elif i==3:
				CaptureCommand = "CAPTURE" + str(self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1) + "PWM"
			elif i==4:
				CaptureCommand = "CAPTUREPWM" + '%02d'%int(self.lstCaptureFrame.get())
			elif i==5:
				CaptureCommand = "CAPTURE" + str(self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1) + "PWM" + '%02d'%int(self.lstCaptureFrame.get())

			print (CaptureCommand) #log
                        
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, CaptureCommand.encode(), buffer)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return
			
			messagebox.showinfo('Done', 'Capture Successful')
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnCaptureEasy_Click(self):

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
		DevPath = ('/dev/' + self.lstPorts.get()).encode('ascii')

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			# Get Capture Command
			isPWM = 0
			CaptureRange = 0
			CapturePWMFrames = 0
			i = self.lstCaptureMode_Options.index(self.lstCaptureMode.get())
			if i==0:
				isPWM = 0
				CaptureRange = 0
			elif i==1:
				isPWM = 0
				CaptureRange = self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1
			elif i==2:
				isPWM = 1
				CaptureRange = 0
				CapturePWMFrames = 0
			elif i==3:
				isPWM = 1
				CaptureRange = self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1
				CapturePWMFrames = 0
			elif i==4:
				isPWM = 1
				CaptureRange = 0
				CapturePWMFrames = int(self.lstCaptureFrame.get())
			elif i==5:
				isPWM = 1
				CaptureRange = self.lstCaptureRange_Options.index(self.lstCaptureRange.get()) + 1
				CapturePWMFrames = int(self.lstCaptureFrame.get())

			# Capture from the LED Analyser
			ret = self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, CapturePWMFrames)
			if ret == -1:
				messagebox.showwarning('Error', 'Unable to send the command!')
				self.FeasaCom_Close(DevPath)
				return
			elif ret == 0:
				messagebox.showwarning('Error', 'Timeout detected!')
				self.FeasaCom_Close(DevPath)
				return
			
			messagebox.showinfo('Done', 'Capture Successful')
			
			# close the port
			self.FeasaCom_Close(DevPath)
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
		DevPath = ('/dev/' + self.lstPorts.get()).encode('ascii')

		# get fiber number
		nfibers = int(self.numFibers.get())
		
		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

                        # retrieve measurements of all sensors/fibers
			for f in range(1, nfibers + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_Send(DevPath, b'GETRGBI' + b'%02d'%f, buffer)
				if ret == -1:
					messagebox.showwarning('Error', 'Unable to send the command!')
					self.FeasaCom_Close(DevPath)
					return
				elif ret == 0:
					messagebox.showwarning('Error', 'Timeout detected!')
					self.FeasaCom_Close(DevPath)
					return

				# shows the received data in the screen
				self.txtLog.insert(tk.END, '\r\n' + '%02d'%f + ' ' + buffer.value.decode('ascii'))

			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')
		

app = Application()
app.master.title('Capture modes - (c) Feasa Enterprises Ltd')
app.mainloop()

