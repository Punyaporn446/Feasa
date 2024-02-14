#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: Fast Test (Multi-threaded)
#
#  DESCRIPTION: This example demonstrates how to use the multi-
#  threaded functions provided in the DLL to set up a fast and
#  efficient communication schema for your application.
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
import FeasaTools
from tkinter import messagebox

class Application(ttk.Frame):

	def __init__(self, master=None):
                #variables
		self.lstPorts_Value = 1

		# create window
		ttk.Frame.__init__(self, master)
		self.grid()

		# add widgets to window
		self.createWidgets()
		
		# populate ports list
		ports = []
		for p in range(1, 255):
			if FeasaCom.IsPortAvailable(p) == 1:
				ports.append(str(p))
		self.lstPorts['values'] = ports
		if len(ports) > 0:
			self.lstPorts.set(ports[0])
		

	def createWidgets(self):
		# controls
		self.frameSetup = ttk.LabelFrame(self, text='Communications Setup', width='183', height='200')
		self.lblCOM = ttk.Label(self.frameSetup, text='COM Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameSetup, width='6', textvariable=self.lstPorts_Value, state='readonly')
		self.btnAdd = ttk.Button(self.frameSetup, text='Add to test', command=self.btnAdd_Click, width='13')
		self.lblDeviceList = ttk.Label(self.frameSetup, text='Ports to test:', justify='left')
		self.lstPortsToTest = tk.Listbox(self.frameSetup, width='26', height='5')
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='183', height='110')
		self.lblExecutionTime = ttk.Label(self.frameCommands, text='Execution time: -', justify='left')
		self.btnRead = ttk.Button(self.frameCommands, text='CAPTURE & TEST', command=self.btnRead_Click, width='25')
		self.frameResponse = ttk.LabelFrame(self, text='Response', width='250', height='320')
		self.txtLog = tk.Text(self.frameResponse, wrap='word', width='28', height='18', background='white', borderwidth='1')

		# position and resize
		self.frameSetup.grid_propagate(0)
		self.frameSetup.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, columnspan=2, padx=5, sticky='W')
		self.lstPorts.grid(row=1, column=0, padx=5, sticky='E')
		self.btnAdd.grid(row=1, column=1, padx=5, sticky='W')
		self.lblDeviceList.grid(row=2, column=0, columnspan=2, padx=5, pady=5, sticky='W')
		self.lstPortsToTest.grid(row=3, column=0, columnspan=2, padx=5, pady=5, sticky='W')
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=1, column=0, pady=5, padx=5)
		self.lblExecutionTime.grid(row=0, column=0, pady=3, padx=5, sticky='W')
		self.btnRead.grid(row=6, column=0, columnspan=2, pady=3, padx=5)
		self.frameResponse.grid_propagate(0)
		self.frameResponse.grid(row=0, rowspan=2, column=1, padx=5)
		self.txtLog.grid(pady=3, padx=5)


	def btnAdd_Click(self):
                # check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# add serial number to list
		self.lstPortsToTest.insert(0,self.lstPorts.get())


	def btnRead_Click(self):
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 32
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if Any Analyser has been added to the bus
		PortCount = self.lstPortsToTest.size()
		if PortCount < 1:
			messagebox.showinfo('No ports added', 'Please, add at least one port to the list of devices to be tested.')
			return

		# Initialize variables
		Ports = pPorts = None
		[Ports, pPorts] = FeasaCom.CreateCArrayOfInt(PortCount, -1)
		SNs = []
		for i in range(0, PortCount):
			Ports[i] = int(self.lstPortsToTest.get(i))
			SNs.append("")
		Responses = pResponses = None
		[Responses, pResponses] = FeasaCom.CreateCArrayOfStrings(PortCount, 500)
		ReturnValues = pReturnValues = None
		[ReturnValues, pReturnValues] = FeasaCom.CreateCArrayOfInt(PortCount, -1)

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		start_time = time.time()

		if (FeasaCom.Open_Multi(pReturnValues, Ports, PortCount, b"57600") == 1):
			#No error

			#Retrieve Serial numbers connected
			for i in range(0, PortCount):
				resp = FeasaCom.GetSNByPort(buffer, Ports[i])
				if resp == 1:
					SNs[i] = buffer.value.decode('ascii')
				else:
					SNs[i] = ""

			#Send command to All Analysers connected
			resp = FeasaCom.SendToAll(pReturnValues, b"CAPTURE", pResponses)
			if (resp != 1):
				for i in range(0, PortCount):
					if (ReturnValues[i] == -1):
						messagebox.showwarning("Error", "Unable to send the command to " + SNs[i] + "!")
						FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
						return;
					elif (ReturnValues[i] == 0):
						messagebox.showwarning("Error", "Timeout or Syntax error detected in " + SNs[i] + "!")
						FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
						return;

			#Send command to All Analysers connected
			resp = FeasaCom.SendToAll(pReturnValues, b"GETHSIALL", pResponses);
			if (resp != 1):
				for i in range(0, PortCount):
					if (ReturnValues[i] == -1):
						messagebox.showwarning("Error", "Unable to send the command to " + SNs[i] + "!")
						FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
						return;
				
					elif (ReturnValues[i] == 0):
						messagebox.showwarning("Error", "Timeout or Syntax error detected in " + SNs[i] + "!")
						FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
						return;

			#Extract response lines and parse responses
			for i in range(0, PortCount):
				#Declare numerical arrays for responses with 20 elements (maximum number of channels)
				HueValues = (ctypes.c_float * 20)()
				SaturationValues = (ctypes.c_int * 20)()
				IntensityValues = (ctypes.c_int * 20)()
				nLines = FeasaTools.Parse_HSI_All(Responses[i], HueValues, SaturationValues, IntensityValues)
				if (nLines > 0):
					for f in range(0, nLines):
						self.txtLog.insert(tk.END, SNs[i] + " %02d"%(f + 1) + " %3.2f"%HueValues[f] + " %03d"%SaturationValues[f] + " %05d"%IntensityValues[f] + '\r\n')
			
			#Close the port
			FeasaCom.Close_Multi(ReturnValues, Ports, PortCount);

                        #Calculates elapsed time
			elapsed_time = (time.time() - start_time) * 1000.0
			self.lblExecutionTime['text'] = "Execution time: %3.1f ms"%elapsed_time
		else:
			#Error: unable to open the selected port
			messagebox.showwarning("Error", "Unable to open all ports")
			
app = Application()
app.master.title('Fast Test MT - (c) Feasa Enterprises Ltd')
app.mainloop()

