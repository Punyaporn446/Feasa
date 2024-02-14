#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: Sequence (blinking LED)
#
#  DESCRIPTION: This example demonstrates how to use Sequence
#  functions provided in the DLL to test a blinking LED
#  so that the light pattern could be tracked.
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
GRAPH_HEIGHT = 120

class Application(ttk.Frame):

	def __init__(self, master=None):

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()
		
		#variables
		self.lstBlinkingSpeed_Value = 6
		self.lstSignalSpeed_Value = 5
		self.chkTimeResImportant_Value = tk.IntVar()
		self.chkTimeResImportant_Value.set(1)
		self.numCycles_Value = tk.StringVar()
		self.numCycles_Value.set('4')
		self.numCaptureTime_Value = tk.StringVar()
		self.numCaptureTime_Value.set('4')
		self.numWaitTime_Value = tk.StringVar()
		self.numSampleCount_Value = tk.StringVar()
		self.numSampleCount_Value.set('100')

		# add widgets to window
		self.createWidgets()
		

	def createWidgets(self):
		# --- Communications frame ---
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='220', height='250')
		self.lblCOM = ttk.Label(self.frameSetup, text='COM Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='10', state='readonly')
		self.lblInfo = ttk.Label(self.frameSetup, text='This example explores the Sequence functionality of the LED Analyser, by allowing the user to perform a Sequence test, and also to find some possible test parameters through the functions provided in the DLL.\n\nFor simplicity, this example is limited to test one channel at a time, but the LED Analuser and DLL can measure all channels simultaneously, if required.', justify='left', wraplength=210)

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
		
		# --- Find Parameters frame ---
		self.frameFindParams = ttk.LabelFrame(self, text='Commands', width='270', height='250')
		self.lblBS = ttk.Label(self.frameFindParams, text='Blinking Speed', justify='left')
		self.lstBlinkingSpeed = ttk.Combobox(self.frameFindParams, width='17', textvariable=self.lstBlinkingSpeed_Value, state='readonly')
		self.lblSS = ttk.Label(self.frameFindParams, text='Signal Speed', justify='left')
		self.lstSignalSpeed = ttk.Combobox(self.frameFindParams, width='17', textvariable=self.lstSignalSpeed_Value, state='readonly')
		self.lblMC = ttk.Label(self.frameFindParams, text='Min cycles to capture:', justify='left')
		self.numCycles = tk.Spinbox(self.frameFindParams, from_=1, to=50, increment=1, width='4', textvariable=self.numCycles_Value)
		self.lblLC = ttk.Label(self.frameFindParams, text='Total LED Count:', justify='left')
		self.numLEDCount = tk.Spinbox(self.frameFindParams, from_=1, to=20, increment=1, width='4')
		self.lblFT = ttk.Label(self.frameFindParams, text='Fiber to test:', justify='left')
		self.numFiberToTest = tk.Spinbox(self.frameFindParams, from_=1, to=20, increment=1, width='4')
		self.chkTimeResImportant = tk.Checkbutton(self.frameFindParams, text="Time resolution is important", variable=self.chkTimeResImportant_Value)
		self.btnFindParams = ttk.Button(self.frameFindParams, text='FIND PARAMETERS', command=self.btnFindParams_Click, width='40')
		
		self.frameFindParams.grid_propagate(0)
		self.frameFindParams.grid(row=0, column=1, pady=5, padx=5)
		self.lblBS.grid(row=0, column=0, pady=3, padx=5, sticky='W')
		self.lstBlinkingSpeed.grid(row=0, column=1, pady=3, padx=5, sticky='W')
		self.lblSS.grid(row=1, column=0, pady=3, padx=5, sticky='W')
		self.lstSignalSpeed.grid(row=1, column=1, pady=3, padx=5, sticky='W')
		self.lblMC.grid(row=2, column=0, pady=3, padx=5, sticky='W')
		self.numCycles.grid(row=2, column=1, pady=3, padx=5, sticky='W')
		self.lblLC.grid(row=3, column=0, pady=3, padx=5, sticky='W')
		self.numLEDCount.grid(row=3, column=1, pady=3, padx=5, sticky='W')
		self.lblFT.grid(row=4, column=0, pady=3, padx=5, sticky='W')
		self.numFiberToTest.grid(row=4, column=1, pady=3, padx=5, sticky='W')
		self.chkTimeResImportant.grid(row=5, column=0, columnspan=2, pady=3, padx=5, sticky='W')
		self.btnFindParams.grid(row=7, column=0, columnspan=2, pady=3, padx=5)

		# populate Signal Speed list
		lstSignalSpeedItems = []
		lstSignalSpeedItems.append('VERY LOW (<1Hz)')
		lstSignalSpeedItems.append('LOW (1-3Hz)')
		lstSignalSpeedItems.append('MEDIUM (3-10Hz)')
		lstSignalSpeedItems.append('MODERATE (10-20Hz)')
		lstSignalSpeedItems.append('HIGH (20-40Hz)')
		lstSignalSpeedItems.append('VERY HIGH (>40Hz)')
		self.lstSignalSpeed['values'] = lstSignalSpeedItems
		self.lstSignalSpeed.set(lstSignalSpeedItems[3])
		
		# populate Blinking Speed list
		lstBlinkingSpeedItems = []
		lstBlinkingSpeedItems.append('0: VERY LOW')
		lstBlinkingSpeedItems.append('1: VERY LOW')
		lstBlinkingSpeedItems.append('2: LOW')
		lstBlinkingSpeedItems.append('3: LOW')
		lstBlinkingSpeedItems.append('4: MEDIUM')
		lstBlinkingSpeedItems.append('5: MEDIUM')
		lstBlinkingSpeedItems.append('6: MODERATE (fast blinking)')
		lstBlinkingSpeedItems.append('7: MODERATE (very fast blinking)')
		lstBlinkingSpeedItems.append('8: HIGH (can barely see it)')
		lstBlinkingSpeedItems.append('9: HIGH')
		lstBlinkingSpeedItems.append('10: VERY HIGH (can\'t see it)')
		self.lstBlinkingSpeed['values'] = lstBlinkingSpeedItems
		self.lstBlinkingSpeed.set(lstBlinkingSpeedItems[6])

		# --- Sequence Test frame ---
		self.frameSequenceTest = ttk.LabelFrame(self, text='Sequence Test', width='200', height='250')
		self.lblFiber = ttk.Label(self.frameSequenceTest, text='Fiber to test:', justify='left')
		self.numFiber = tk.Spinbox(self.frameSequenceTest, from_=1, to=20, increment=1, width='4')
		self.lblSD = ttk.Label(self.frameSequenceTest, text='Start delay:', justify='left')
		self.numStartDelay = tk.Spinbox(self.frameSequenceTest, from_=0, to=999, increment=1, width='4')
		self.lblCT = ttk.Label(self.frameSequenceTest, text='Capture time:', justify='left')
		self.numCaptureTime = tk.Spinbox(self.frameSequenceTest, from_=1, to=999, increment=1, width='4', textvariable=self.numCaptureTime_Value)
		self.lblWT = ttk.Label(self.frameSequenceTest, text='Wait time:', justify='left')
		self.numWaitTime = tk.Spinbox(self.frameSequenceTest, from_=0, to=999, increment=1, width='4', textvariable=self.numWaitTime_Value)
		self.lblSC = ttk.Label(self.frameSequenceTest, text='Sample count:', justify='left')
		self.numSampleCount = tk.Spinbox(self.frameSequenceTest, from_=1, to=9999, increment=25, width='4', textvariable=self.numSampleCount_Value)
		self.btnSequenceTest = ttk.Button(self.frameSequenceTest, text='SEQUENCE TEST', command=self.btnSequenceTest_Click, width='29')
		
		self.frameSequenceTest.grid_propagate(0)
		self.frameSequenceTest.grid(row=0, column=2, padx=5)
		self.lblFiber.grid(row=0, column=0, pady=3, padx=5, sticky='W')
		self.numFiber.grid(row=0, column=1, pady=3, padx=5)
		self.lblSD.grid(row=1, column=0, pady=3, padx=5, sticky='W')
		self.numStartDelay.grid(row=1, column=1, pady=3, padx=5)
		self.lblCT.grid(row=2, column=0, pady=3, padx=5, sticky='W')
		self.numCaptureTime.grid(row=2, column=1, pady=3, padx=5)
		self.lblWT.grid(row=3, column=0, pady=3, padx=5, sticky='W')
		self.numWaitTime.grid(row=3, column=1, pady=3, padx=5)
		self.lblSC.grid(row=4, column=0, pady=3, padx=5, sticky='W')
		self.numSampleCount.grid(row=4, column=1, pady=3, padx=5)
		self.btnSequenceTest.grid(row=5, column=0, columnspan=2, pady=3, padx=5)

		# --- Charts ---
		self.lblgraphIntensity = ttk.Label(self, text='Intensity:', justify='left')
		self.graphIntensity = tk.Canvas(self, width=GRAPH_WIDTH, height=GRAPH_HEIGHT)
		self.graphIntensity.grid_propagate(1)
		self.lblgraphIntensity.grid(row=1, column=0, columnspan=3, pady=5, padx=15, sticky='W')
		self.graphIntensity.grid(row=2, column=0, columnspan=3, pady=5, padx=5)
		self.graphIntensity.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')

		self.lblgraphCIE = ttk.Label(self, text='CIE1931 xy:', justify='left')
		self.graphCIE = tk.Canvas(self, width=GRAPH_WIDTH, height=GRAPH_HEIGHT)
		self.graphCIE.grid_propagate(1)
		self.lblgraphCIE.grid(row=3, column=0, columnspan=3, pady=5, padx=15, sticky='W')
		self.graphCIE.grid(row=4, column=0, columnspan=3, pady=5, padx=5)
		self.graphCIE.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')


	def btnFindParams_Click(self):
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

		# get settings
		SignalSpeed = self.lstSignalSpeed.current() * 2
		BlinkingSpeed = self.lstBlinkingSpeed.current()
		TotalLEDCount = int(self.numLEDCount.get())
		Fiber = int(self.numFiberToTest.get())
		MinCycleCount = int(self.numCycles.get())
		TimeResolutionIsImportant = int(self.chkTimeResImportant_Value.get())

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		FeasaCom.SetResponseTimeout(8000) # 8000 milliseconds

		# open port
		ret = FeasaCom.Open(DevicePort, b"57600")
		if ret == 1:
			#No error

			CaptureTime = (ctypes.c_int)()
			WaitTime = (ctypes.c_int)()
			SampleCount = (ctypes.c_int)()

			# Find out test settings
			ret = FeasaCom.Sequence_FindTestSettings(DevicePort, TotalLEDCount, Fiber, SignalSpeed, BlinkingSpeed, MinCycleCount, TimeResolutionIsImportant, CaptureTime, WaitTime, SampleCount)
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return
			
			# Set results
			self.numCaptureTime_Value.set(str(CaptureTime.value))
			self.numWaitTime_Value.set(str(WaitTime.value))
			self.numSampleCount_Value.set(str(SampleCount.value))
			
			messagebox.showinfo("Succeeded", "Parameters calculated successfully")
			
			# Close the port
			FeasaCom.Close(DevicePort)
		else:
			#Error: unable to open the selected port
			messagebox.showwarning("Error", "Unable to open all ports")



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
		Fiber = int(self.numFiber.get())
		StartDelay = int(self.numStartDelay.get())
		CaptureTime = int(self.numCaptureTime.get())
		WaitTime = int(self.numWaitTime.get())
		SampleCount = int(self.numSampleCount.get())

		# initialize variables
		xValues = (ctypes.c_float * SampleCount)()
		yValues = (ctypes.c_float * SampleCount)()
		IntensityValues = (ctypes.c_int * SampleCount)()

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
			ret = FeasaCom.Sequence_Capture(DevicePort, Fiber)
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return
			
			# Read back results
			ret = FeasaCom.Sequence_ReadxyI(DevicePort, Fiber, xValues, yValues, IntensityValues)
			if ret != 1:
				FeasaCom.GetError_Description(buffer)
				messagebox.showwarning('Error', buffer.value.decode('ascii'))
				FeasaCom.Close(DevicePort)
				return

			# plot: draw white background (fixed dimensions)
			self.graphIntensity.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')
			# plot: draw graph
			MaxInt = max(IntensityValues) * 1.1
			xincr = float(GRAPH_WIDTH) / float(SampleCount - 1)
			x1 = 0
			y1 = 0
			for i in range(0, SampleCount-1):
				x2 = xincr * i
				y2 = GRAPH_HEIGHT - (float(IntensityValues[i]) * GRAPH_HEIGHT / MaxInt)
				self.graphIntensity.create_line(x1, y1, x2, y2, fill='red')
				x1 = x2
				y1 = y2

			# plot: draw white background (fixed dimensions)
			self.graphCIE.create_rectangle(0, 0, GRAPH_WIDTH, GRAPH_HEIGHT, fill='white')
			# plot: draw graph
			xincr = float(GRAPH_WIDTH) / float(SampleCount - 1)
			x1 = 0
			y1 = 0
			for i in range(0, SampleCount-1):
				x2 = xincr * i
				y2 = GRAPH_HEIGHT - (float(xValues[i]) * GRAPH_HEIGHT)
				self.graphCIE.create_line(x1, y1, x2, y2, fill='red')
				x1 = x2
				y1 = y2
			x1 = 0
			y1 = 0
			for i in range(0, SampleCount-1):
				x2 = xincr * i
				y2 = GRAPH_HEIGHT - (float(yValues[i]) * GRAPH_HEIGHT)
				self.graphCIE.create_line(x1, y1, x2, y2, fill='blue')
				x1 = x2
				y1 = y2
			
			# Close the port
			FeasaCom.Close(DevicePort)
		else:
			#Error: unable to open the selected port
			messagebox.showwarning("Error", "Unable to open all ports")
		

app = Application()
app.master.title('Sequence - (c) Feasa Enterprises Ltd')
app.mainloop()

