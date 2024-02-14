#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: Sequence (sweeping light)
#
#  DESCRIPTION: This example demonstrates how To use Sequence
#  functions provided In the DLL To test a sweeping light
#  pattern from different LEDs, extracting intensity and
#  pattern times afterwards.
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

from tkinter import ttk
import tkinter as tk
import ctypes
import time
import FeasaCom
from tkinter import messagebox

TOFLASH = 0
GRAPH_WIDTH = 690
GRAPH_HEIGHT = 350

class Application(ttk.Frame):

	def __init__(self, master=None):

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()
		
		#variables
		self.numFibers_Value = tk.StringVar()
		self.numFibers_Value.set('12')
		self.chkIsOffToOnPattern_Value = tk.IntVar()
		self.chkIsOffToOnPattern_Value.set(1)
		self.numCaptureTime_Value = tk.StringVar()
		self.numCaptureTime_Value.set('5')
		self.numWaitTime_Value = tk.StringVar()
		self.numSampleCount_Value = tk.StringVar()
		self.numSampleCount_Value.set('200')

		# add widgets to window
		self.createWidgets()
		

	def createWidgets(self):
		# --- Communications frame ---
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='200', height='250')
		self.lblCOM = ttk.Label(self.frameSetup, text='COM Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='8', state='readonly')
		self.lblInfo = ttk.Label(self.frameSetup, text='This example explores the Sequence functionality applied to the more and more common automotive sweeping indicators.', justify='left', wraplength=190)

		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5, sticky='E')
		self.lstPorts.grid(row=0, column=1, padx=5, sticky='E')
		self.lblInfo.grid(row=2, column=0, columnspan=2, padx=5, pady=5, sticky='W')
		
		# populate ports list
		ports = []
		for p in range(1, 255):
			if FeasaCom.IsPortAvailable(p) == 1:
				ports.append(str(p))
		self.lstPorts['values'] = ports
		if len(ports) > 0:
			self.lstPorts.set(ports[len(ports) - 1])

		# --- Sequence Test frame ---
		self.frameSequenceTest = ttk.LabelFrame(self, text='Sequence Test', width='220', height='250')
		self.lblFiber = ttk.Label(self.frameSequenceTest, text='Fiber to test:', justify='left')
		self.numFibers = tk.Spinbox(self.frameSequenceTest, from_=1, to=20, increment=1, width='4', textvariable=self.numFibers_Value)
		self.lblFiber2 = ttk.Label(self.frameSequenceTest, text='(test 1 to n)', justify='left')
		self.lblSD = ttk.Label(self.frameSequenceTest, text='Start delay:', justify='left')
		self.numStartDelay = tk.Spinbox(self.frameSequenceTest, from_=0, to=999, increment=1, width='4')
		self.lblCT = ttk.Label(self.frameSequenceTest, text='Capture time:', justify='left')
		self.numCaptureTime = tk.Spinbox(self.frameSequenceTest, from_=1, to=999, increment=1, width='4', textvariable=self.numCaptureTime_Value)
		self.lblWT = ttk.Label(self.frameSequenceTest, text='Wait time:', justify='left')
		self.numWaitTime = tk.Spinbox(self.frameSequenceTest, from_=0, to=999, increment=1, width='4', textvariable=self.numWaitTime_Value)
		self.lblSC = ttk.Label(self.frameSequenceTest, text='Sample count:', justify='left')
		self.numSampleCount = tk.Spinbox(self.frameSequenceTest, from_=1, to=3500, increment=25, width='4', textvariable=self.numSampleCount_Value)
		self.chkIsOffToOnPattern = tk.Checkbutton(self.frameSequenceTest, text="Is Off-to-On pattern", variable=self.chkIsOffToOnPattern_Value)
		self.btnSequenceTest = ttk.Button(self.frameSequenceTest, text='SEQUENCE TEST', command=self.btnSequenceTest_Click, width='30')
		
		self.frameSequenceTest.grid_propagate(0)
		self.frameSequenceTest.grid(row=0, column=1, padx=5)
		self.lblFiber.grid(row=0, column=0, pady=3, padx=5, sticky='W')
		self.numFibers.grid(row=0, column=1, pady=3, padx=5)
		self.lblFiber2.grid(row=0, column=2, pady=3, padx=5, sticky='W')
		self.lblSD.grid(row=1, column=0, pady=3, padx=5, sticky='W')
		self.numStartDelay.grid(row=1, column=1, pady=3, padx=5)
		self.lblCT.grid(row=2, column=0, pady=3, padx=5, sticky='W')
		self.numCaptureTime.grid(row=2, column=1, pady=3, padx=5)
		self.lblWT.grid(row=3, column=0, pady=3, padx=5, sticky='W')
		self.numWaitTime.grid(row=3, column=1, pady=3, padx=5)
		self.lblSC.grid(row=4, column=0, pady=3, padx=5, sticky='W')
		self.numSampleCount.grid(row=4, column=1, pady=3, padx=5)
		self.chkIsOffToOnPattern.grid(row=5, column=0, columnspan=3, pady=3, padx=5, sticky='W')
		self.btnSequenceTest.grid(row=6, column=0, columnspan=3, pady=3, padx=5)

		# --- Timing frame ---
		self.frameResponse = ttk.LabelFrame(self, text='Timing', width='270', height='250')
		self.txtTimes = tk.Text(self.frameResponse, wrap='word', width='31', height='13', background='white', borderwidth='1')
		self.frameResponse.grid_propagate(0)
		self.frameResponse.grid(row=0, column=2, padx=5)
		self.txtTimes.grid(pady=3, padx=5)
		
		# --- Charts ---
		self.lblgraphIntensity = ttk.Label(self, text='Intensity:', justify='left')
		self.graphIntensity = tk.Canvas(self, width=GRAPH_WIDTH, height=GRAPH_HEIGHT)
		self.graphIntensity.grid_propagate(1)
		self.lblgraphIntensity.grid(row=1, column=0, columnspan=3, pady=5, padx=15, sticky='W')
		self.graphIntensity.grid(row=2, column=0, columnspan=3, pady=5, padx=5)
		self.graphIntensity.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')


	def btnSequenceTest_Click(self):
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 255
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if Any Analyser has been added to the bus
		PortCount = len(self.lstPorts['values'])
		if PortCount < 1:
			messagebox.showinfo('No ports detected!', 'No ports have been detected!')
			return
		
		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return
		
		# get selected port
		DevicePort = int(self.lstPorts.get())

		# get test parameters
		FibersToTest = int(self.numFibers.get())
		StartDelay = int(self.numStartDelay.get())
		CaptureTime = int(self.numCaptureTime.get())
		WaitTime = int(self.numWaitTime.get())
		SampleCount = int(self.numSampleCount.get())
		isOffToOnPattern = int(self.chkIsOffToOnPattern_Value.get())

		# initialize variables
		IntensityValues = (ctypes.c_int * SampleCount)()
		LowTimes = (ctypes.c_int * FibersToTest)()
		HighTimes = (ctypes.c_int * FibersToTest)()
		tIntensityValues = (ctypes.c_int * FibersToTest)()

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		FeasaCom.SetResponseTimeout(8000) # 8000 milliseconds

		# open port
		ret = FeasaCom.Open(DevicePort, b"57600")
		if ret == 1:
			#No error

			# Set up Sequence
			ret = FeasaCom.Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH)
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return

			# Perform sequence capture
			ret = FeasaCom.Sequence_Capture(DevicePort, 0) #0: test all fibers
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return

			# Clear graph
			self.graphIntensity.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')
			
			# Read back results
			for i in range(0, FibersToTest):
				ret = FeasaCom.Sequence_ReadIntensity(DevicePort, i + 1, IntensityValues)
				if ret != 1:
					FeasaCom.GetError_Description(buffer)
					messagebox.showwarning('Error', buffer.value.decode('ascii'))
					FeasaCom.Close(DevicePort)
					return

				# plot: draw graph
				MaxInt = float(max(IntensityValues)) * 1.1
				xincr = float(GRAPH_WIDTH) / float(SampleCount - 1)
				yincr = float(GRAPH_HEIGHT) / float(FibersToTest)
				yoffset = yincr * (FibersToTest - i)
				x1 = 0
				y1 = 0
				for s in range(0, SampleCount):
					x2 = xincr * s
					y2 = yoffset - (float(IntensityValues[s]) * yincr / MaxInt)
					self.graphIntensity.create_line(x1, y1, x2, y2, fill='red')
					x1 = x2
					y1 = y2

			# Retrieve LED times
			ret = FeasaCom.Sequence_GetSweepingPattern(DevicePort, FibersToTest, isOffToOnPattern, LowTimes, HighTimes, tIntensityValues)
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return

			# Print pattern times
			self.txtTimes.delete(1.0, tk.END)
			self.txtTimes.insert(tk.END, 'Fib Low(ms) High(ms) Int\r\n')
			for f in range(0, FibersToTest):
				self.txtTimes.insert(tk.END, "%02d"%(f + 1) + "  %03d"%LowTimes[f] + "     %03d"%HighTimes[f] + "      %05d"%tIntensityValues[f] + '\r\n')
			
			# Close the port
			FeasaCom.Close(DevicePort)
		else:
			#Error: unable to open the selected port
			messagebox.showwarning("Error", "Unable to open all ports")
		

app = Application()
app.master.title('Sequence - (c) Feasa Enterprises Ltd')
app.mainloop()

