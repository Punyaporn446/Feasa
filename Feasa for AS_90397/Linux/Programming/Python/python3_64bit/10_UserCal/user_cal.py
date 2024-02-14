#**************************************************************
#
#  (c) Feasa Enterprises Ltd
#  LED Analyser examples
#  Developed by: Carles Martínez Rius
#
#  PROJECT: UserCal
#
#  DESCRIPTION: This example demonstrates how to use the
#  UserCal library embedded in the Feasa SO Library in order
#  to ease the integration of the calibration process in any
#  user custom software.
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
		
		#variables
		self.lstPorts_Value = 1
		self.lstCaptureRange_Value = "MEDIUM"
		self.toFlashVar = tk.IntVar()
		self.FRAMES = 5
		self.PWMCheckVar = tk.IntVar()
		self.AbsIntRefVar = tk.StringVar(value='2.355E-02')
		self.WavelengthRefVar = tk.StringVar(value='638')
		self.xRefVar = tk.StringVar(value='0.7011')
		self.yRefVar = tk.StringVar(value='0.2912')
		self.xRefRVar = tk.StringVar(value='0.7071')
		self.yRefRVar = tk.StringVar(value='0.2927')
		self.AbsIntRefRVar = tk.StringVar(value='1.28569')
		self.xRefGVar = tk.StringVar(value='0.1827')
		self.yRefGVar = tk.StringVar(value='0.7177')
		self.AbsIntRefGVar = tk.StringVar(value='2.20375')
		self.xRefBVar = tk.StringVar(value='0.1427')
		self.yRefBVar = tk.StringVar(value='0.0388')
		self.AbsIntRefBVar = tk.StringVar(value='0.550639')

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
		self.FeasaCom_UserCal_ResetIntensity = self.FeasaLIB['FeasaCom_UserCal_ResetIntensity']
		self.FeasaCom_UserCal_ResetIntensity.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_ResetIntensity.restype = ctypes.c_int
		self.FeasaCom_UserCal_GetIntensityGain = self.FeasaLIB['FeasaCom_UserCal_GetIntensityGain']
		self.FeasaCom_UserCal_GetIntensityGain.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
		self.FeasaCom_UserCal_GetIntensityGain.restype = ctypes.c_int
		self.FeasaCom_UserCal_SetIntensityGain = self.FeasaLIB['FeasaCom_UserCal_SetIntensityGain']
		self.FeasaCom_UserCal_SetIntensityGain.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_SetIntensityGain.restype = ctypes.c_int
		self.FeasaCom_UserCal_AdjustIntensity = self.FeasaLIB['FeasaCom_UserCal_AdjustIntensity']
		self.FeasaCom_UserCal_AdjustIntensity.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_AdjustIntensity.restype = ctypes.c_int
		self.FeasaCom_UserCal_ResetAbsInt = self.FeasaLIB['FeasaCom_UserCal_ResetAbsInt']
		self.FeasaCom_UserCal_ResetAbsInt.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_ResetAbsInt.restype = ctypes.c_int
		self.FeasaCom_UserCal_GetAbsIntFactor = self.FeasaLIB['FeasaCom_UserCal_GetAbsIntFactor']
		self.FeasaCom_UserCal_GetAbsIntFactor.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_double)]
		self.FeasaCom_UserCal_GetAbsIntFactor.restype = ctypes.c_int
		self.FeasaCom_UserCal_SetAbsIntFactor = self.FeasaLIB['FeasaCom_UserCal_SetAbsIntFactor']
		self.FeasaCom_UserCal_SetAbsIntFactor.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_double, ctypes.c_int]
		self.FeasaCom_UserCal_SetAbsIntFactor.restype = ctypes.c_int
		self.FeasaCom_UserCal_AdjustAbsInt = self.FeasaLIB['FeasaCom_UserCal_AdjustAbsInt']
		self.FeasaCom_UserCal_AdjustAbsInt.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_double, ctypes.c_int]
		self.FeasaCom_UserCal_AdjustAbsInt.restype = ctypes.c_int
		self.FeasaCom_UserCal_ResetWavelengthOffset = self.FeasaLIB['FeasaCom_UserCal_ResetWavelengthOffset']
		self.FeasaCom_UserCal_ResetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_ResetWavelengthOffset.restype = ctypes.c_int
		self.FeasaCom_UserCal_GetWavelengthOffset = self.FeasaLIB['FeasaCom_UserCal_GetWavelengthOffset']
		self.FeasaCom_UserCal_GetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
		self.FeasaCom_UserCal_GetWavelengthOffset.restype = ctypes.c_int
		self.FeasaCom_UserCal_SetWavelengthOffset = self.FeasaLIB['FeasaCom_UserCal_SetWavelengthOffset']
		self.FeasaCom_UserCal_SetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_SetWavelengthOffset.restype = ctypes.c_int
		self.FeasaCom_UserCal_AdjustWavelengthOffset = self.FeasaLIB['FeasaCom_UserCal_AdjustWavelengthOffset']
		self.FeasaCom_UserCal_AdjustWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_AdjustWavelengthOffset.restype = ctypes.c_int
		self.FeasaCom_UserCal_ResetxyOffsets = self.FeasaLIB['FeasaCom_UserCal_ResetxyOffsets']
		self.FeasaCom_UserCal_ResetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
		self.FeasaCom_UserCal_ResetxyOffsets.restype = ctypes.c_int
		self.FeasaCom_UserCal_GetxyOffsets = self.FeasaLIB['FeasaCom_UserCal_GetxyOffsets']
		self.FeasaCom_UserCal_GetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
		self.FeasaCom_UserCal_GetxyOffsets.restype = ctypes.c_int
		self.FeasaCom_UserCal_SetxyOffsets = self.FeasaLIB['FeasaCom_UserCal_SetxyOffsets']
		self.FeasaCom_UserCal_SetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
		self.FeasaCom_UserCal_SetxyOffsets.restype = ctypes.c_int
		self.FeasaCom_UserCal_AdjustxyOffsets = self.FeasaLIB['FeasaCom_UserCal_AdjustxyOffsets']
		self.FeasaCom_UserCal_AdjustxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
		self.FeasaCom_UserCal_AdjustxyOffsets.restype = ctypes.c_int
		self.FeasaCom_GetError_Description = self.FeasaLIB['FeasaCom_GetError_Description']
		self.FeasaCom_GetError_Description.argtypes = [ctypes.c_char_p]
		self.FeasaCom_GetError_Description.restype = ctypes.c_int
		self.FeasaCom_UserCal_ResetRGBAdj = self.FeasaLIB['FeasaCom_UserCal_ResetRGBAdj']
		self.FeasaCom_UserCal_ResetRGBAdj.argtypes = [ctypes.c_char_p, ctypes.c_int]
		self.FeasaCom_UserCal_ResetRGBAdj.restype = ctypes.c_int
		self.FeasaCom_UserCal_TakeRGBCurrentValues = self.FeasaLIB['FeasaCom_UserCal_TakeRGBCurrentValues']
		self.FeasaCom_UserCal_TakeRGBCurrentValues.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_char]
		self.FeasaCom_UserCal_TakeRGBCurrentValues.restype = ctypes.c_int
		self.FeasaCom_UserCal_AdjustRGB = self.FeasaLIB['FeasaCom_UserCal_AdjustRGB']
		self.FeasaCom_UserCal_AdjustRGB.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double]
		self.FeasaCom_UserCal_AdjustRGB.restype = ctypes.c_int
		
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
		self.frameCOM = ttk.LabelFrame(self, text='Communications Setup', width='220', height='50')
		self.lblCOM = ttk.Label(self.frameCOM, text='Port: ', justify='left')
		self.lstPorts = ttk.Combobox(self.frameCOM, width='10', textvariable=self.lstPorts_Value, state='readonly')
		self.frameCapture = ttk.LabelFrame(self, text='Capture', width='220', height='400')
		self.lblFibers = ttk.Label(self.frameCapture, text='Fibers count:', justify='left')
		self.numFibers = tk.Spinbox(self.frameCapture, from_=5, to=20, increment=1, width='3')
		self.lblCaptureRange = ttk.Label(self.frameCapture, text='Range: ', justify='left')
		self.lstCaptureRange = ttk.Combobox(self.frameCapture, width='10', textvariable=self.lstCaptureRange_Value, state='readonly', values=["AUTO", "LOW", "MEDIUM", "HIGH", "SUPER", "ULTRA"])
		self.chkPWM = ttk.Checkbutton(self.frameCapture, width='10', text='PWM', variable=self.PWMCheckVar, onvalue=1, offvalue=0)
		self.lbltoFlash = ttk.Label(self.frameCapture, text='Save Parameters to: ', justify='left')
		self.optRAM = ttk.Radiobutton(self.frameCapture, width='10', text='RAM', variable=self.toFlashVar, value=0)
		self.optFLASH = ttk.Radiobutton(self.frameCapture, width='10', text='Flash', variable=self.toFlashVar, value=1)
		self.frameCommands = ttk.LabelFrame(self, text='Commands', width='300', height='460')
		self.btnBalanceInt = ttk.Button(self.frameCommands, text='Balance Int to avg', command=self.btnBalanceInt_Click, width='23')
		self.lblRefAbsInt = ttk.Label(self.frameCommands, text='Ref:', justify='left')
		self.txtRefAbsInt = tk.Entry(self.frameCommands, width='14', background='white', borderwidth='1', textvariable=self.AbsIntRefVar)
		self.btnAdjustAbsInt = ttk.Button(self.frameCommands, text='Adjust Abs Int', command=self.btnAdjustAbsInt_Click, width='23')
		self.lblRefWavelength = ttk.Label(self.frameCommands, text='Ref:', justify='left')
		self.txtRefWavelength = tk.Entry(self.frameCommands, width='5', background='white', borderwidth='1', textvariable=self.WavelengthRefVar)
		self.btnAdjustWavelength = ttk.Button(self.frameCommands, text='Adjust Wavelength', command=self.btnAdjustWavelength_Click, width='23')
		self.lblRefxy = ttk.Label(self.frameCommands, text='Ref:', justify='left')
		self.txtRefx = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.xRefVar)
		self.txtRefy = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.yRefVar)
		self.btnAdjustxy = ttk.Button(self.frameCommands, text='Adjust xy', command=self.btnAdjustxy_Click, width='23')
		self.lblRefR = ttk.Label(self.frameCommands, text='Ref R:', justify='left')
		self.txtRefxR = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.xRefRVar)
		self.txtRefyR = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.yRefRVar)
		self.txtRefAbsIntR = tk.Entry(self.frameCommands, width='7', background='white', borderwidth='1', textvariable=self.AbsIntRefRVar)
		self.lblRefG = ttk.Label(self.frameCommands, text='Ref G:', justify='left')
		self.txtRefxG = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.xRefGVar)
		self.txtRefyG = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.yRefGVar)
		self.txtRefAbsIntG = tk.Entry(self.frameCommands, width='7', background='white', borderwidth='1', textvariable=self.AbsIntRefGVar)
		self.lblRefB = ttk.Label(self.frameCommands, text='Ref B:', justify='left')
		self.txtRefxB = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.xRefBVar)
		self.txtRefyB = tk.Entry(self.frameCommands, width='6', background='white', borderwidth='1', textvariable=self.yRefBVar)
		self.txtRefAbsIntB = tk.Entry(self.frameCommands, width='7', background='white', borderwidth='1', textvariable=self.AbsIntRefBVar)
		self.btnAdjustRGB = ttk.Button(self.frameCommands, text='Adjust RGB', command=self.btnAdjustRGB_Click, width='23')
		self.btnReadParams = ttk.Button(self.frameCommands, text='Read back params', command=self.btnReadParams_Click, width='23')
		self.frameData = ttk.LabelFrame(self, text='Data', width='320', height='460')
		self.txtLog = tk.Text(self.frameData, wrap='word', width='35', height='22', background='white', borderwidth='1')
		self.vScrollbar = tk.Scrollbar(self.frameData, orient="vertical", command=self.txtLog.yview)
		self.txtLog.configure(yscrollcommand=self.vScrollbar.set)

		# position and resize
		self.frameCOM.grid_propagate(0)
		self.frameCOM.grid(row=0, column=0, pady=5, padx=5)
		self.lblCOM.grid(row=0, column=0, padx=5)
		self.lstPorts.grid(row=0, column=1)
		self.frameCapture.grid_propagate(0)
		self.frameCapture.grid(row=1, column=0, pady=5, padx=5)
		self.lblFibers.grid(row=0, column=0, pady=25, sticky='E')
		self.numFibers.grid(row=0, column=1, pady=25, sticky='W')
		self.lblCaptureRange.grid(row=1, column=0, pady=3, sticky='E')
		self.lstCaptureRange.grid(row=1, column=1, pady=3)
		self.chkPWM.grid(row=2, column=1, pady=3, sticky='E')
		self.lbltoFlash.grid(row=3, column=0, columnspan=2, pady=3, sticky='W')
		self.optRAM.grid(row=4, column=0, pady=3, sticky='E')
		self.optFLASH.grid(row=4, column=1, pady=3, sticky='W')
		self.frameCommands.grid_propagate(0)
		self.frameCommands.grid(row=0, column=1, rowspan=2, pady=5, padx=5)
		self.btnBalanceInt.grid(row=0, column=0, columnspan=3, pady=8, padx=5)
		self.lblRefAbsInt.grid(row=1, column=0, pady=2, padx=5)
		self.txtRefAbsInt.grid(row=1, column=1, columnspan=2, pady=2, padx=5, sticky='W')
		self.btnAdjustAbsInt.grid(row=2, column=0, columnspan=3, pady=8, padx=5)
		self.lblRefWavelength.grid(row=3, column=0, pady=2, padx=5)
		self.txtRefWavelength.grid(row=3, column=1, pady=2, padx=5, sticky='W')
		self.btnAdjustWavelength.grid(row=4, column=0, columnspan=3, pady=8, padx=5)
		self.lblRefxy.grid(row=5, column=0, pady=2, padx=5)
		self.txtRefx.grid(row=5, column=1, pady=2, padx=2, sticky='W')
		self.txtRefy.grid(row=5, column=2, pady=2, padx=2, sticky='W')
		self.btnAdjustxy.grid(row=6, column=0, columnspan=3, pady=8, padx=5)
		self.lblRefR.grid(row=7, column=0, pady=2, padx=5)
		self.txtRefxR.grid(row=7, column=1, pady=2, padx=2, sticky='W')
		self.txtRefyR.grid(row=7, column=2, pady=2, padx=2, sticky='W')
		self.txtRefAbsIntR.grid(row=7, column=3, pady=2, padx=2, sticky='W')
		self.lblRefG.grid(row=8, column=0, pady=2, padx=5)
		self.txtRefxG.grid(row=8, column=1, pady=2, padx=2, sticky='W')
		self.txtRefyG.grid(row=8, column=2, pady=2, padx=2, sticky='W')
		self.txtRefAbsIntG.grid(row=8, column=3, pady=2, padx=2, sticky='W')
		self.lblRefB.grid(row=9, column=0, pady=2, padx=5)
		self.txtRefxB.grid(row=9, column=1, pady=2, padx=2, sticky='W')
		self.txtRefyB.grid(row=9, column=2, pady=2, padx=2, sticky='W')
		self.txtRefAbsIntB.grid(row=9, column=3, pady=2, padx=2, sticky='W')
		self.btnAdjustRGB.grid(row=10, column=0, columnspan=3, pady=8, padx=5)
		self.btnReadParams.grid(row=11, column=0, columnspan=3, pady=15, padx=5)
		self.frameData.grid_propagate(0)
		self.frameData.grid(row=0, column=2, rowspan=2, pady=5, padx=5)
		self.txtLog.grid_propagate(0)
		self.txtLog.grid(padx=5, row=0, column=0)
		self.vScrollbar.grid(row=0, column=1, sticky='NSW')

		# populate list of Capture Ranges
		self.lstCaptureRange.set("MEDIUM")

	def btnBalanceInt_Click(self):
        
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')
		isPWM = int(self.PWMCheckVar.get())
		CaptureRange = self.lstCaptureRange.current()
		PWMframes = 5
		toFlash = self.toFlashVar.get()

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			#-------------------------------------------------
                        # RELATIVE INTENSITY ADJUSTMENT
                        #-------------------------------------------------
                        # Reset intensities
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_ResetIntensity(DevPath, f, toFlash)

                        # Capture
			self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

			# Calculates average intensity
			AvgInt = 0
			for f in range(1, NFIBERS + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_Send(DevPath, b'GETINTENSITY' + b'%02d'%f, buffer)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return
				AvgInt += int(buffer.value.decode('ascii'))
			AvgInt = int(AvgInt / NFIBERS)
			self.txtLog.insert(tk.END, '\n' + 'AvgInt=' + str(AvgInt))

			# Adjustment
			for f in range(1, NFIBERS + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_UserCal_AdjustIntensity(DevPath, f, AvgInt, isPWM, CaptureRange, toFlash)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return
                        
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETINTENSITYALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\n' + 'Results:' + '\n' + buffer.value.decode('ascii'))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnAdjustAbsInt_Click(self):
        
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')
		isPWM = int(self.PWMCheckVar.get())
		CaptureRange = self.lstCaptureRange.current()
		PWMframes = 5
		toFlash = self.toFlashVar.get()

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			#-------------------------------------------------
                        # ABSOLUTE INTENSITY ADJUSTMENT
                        #-------------------------------------------------
                        # Reset intensities
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_ResetAbsInt(DevPath, f, toFlash)

                        # Capture
			self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

			# Calculates average intensity
			AbsIntRef = 0.0
			try:
				AbsIntRef = float(self.txtRefAbsInt.get())
			except:
				AbsIntRef = 2.355E-02
			self.txtLog.insert(tk.END, '\n' + 'AbsIntRef=' + str(AbsIntRef))

			# Adjustment
			for f in range(1, NFIBERS + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_UserCal_AdjustAbsInt(DevPath, f, AbsIntRef, toFlash)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return
                        
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETABSINTALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\n' + 'Results:' + '\n' + buffer.value.decode('ascii'))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnAdjustWavelength_Click(self):
        
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')
		isPWM = int(self.PWMCheckVar.get())
		CaptureRange = self.lstCaptureRange.current()
		PWMframes = 5
		toFlash = self.toFlashVar.get()

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			#-------------------------------------------------
                        # WAVELENGTH OFFSETS ADJUSTMENT
                        #-------------------------------------------------
                        # Reset intensities
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_ResetWavelengthOffset(DevPath, f, toFlash)

                        # Capture
			self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

			# Calculates average intensity
			Wref = 0
			try:
				Wref = int(self.txtRefWavelength.get())
			except:
				Wref = 638
			self.txtLog.insert(tk.END, '\n' + 'Wref=' + str(Wref))

			# Adjustment
			for f in range(1, NFIBERS + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_UserCal_AdjustWavelengthOffset(DevPath, f, Wref, toFlash)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return
                        
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETwavelengthALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\n' + 'Results:' + '\n' + buffer.value.decode('ascii'))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnAdjustxy_Click(self):
        
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')
		isPWM = int(self.PWMCheckVar.get())
		CaptureRange = self.lstCaptureRange.current()
		PWMframes = 5
		toFlash = self.toFlashVar.get()

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			#-------------------------------------------------
                        # xy OFFSETS ADJUSTMENT
                        #-------------------------------------------------
                        # Reset intensities
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_ResetxyOffsets(DevPath, f, toFlash)

                        # Capture
			self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

			# Calculates average intensity
			xRef = 0.0
			try:
				xRef = float(self.txtRefx.get())
			except:
				xRef = 0.0
			yRef = 0.0
			try:
				yRef = float(self.txtRefy.get())
			except:
				yRef = 0.0
			self.txtLog.insert(tk.END, '\n' + 'xRef=' + str(xRef) + ', yRef=' + str(yRef))

			# Adjustment
			for f in range(1, NFIBERS + 1):
				# Send command to the LED Analyser
				# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
				ret = self.FeasaCom_UserCal_AdjustxyOffsets(DevPath, f, xRef, yRef, toFlash)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return
                        
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETxyALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\n' + 'Results:' + '\n' + buffer.value.decode('ascii'))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnAdjustRGB_Click(self):
        
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')
		isPWM = int(self.PWMCheckVar.get())
		CaptureRange = self.lstCaptureRange.current()
		PWMframes = 5
		toFlash = self.toFlashVar.get()

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			#-------------------------------------------------
                        # RGB ADJUSTMENT
                        #-------------------------------------------------

			# Retrieve values
			xRefR = ctypes.c_float(0.0)
			try:
				xRefR = ctypes.c_float(float(self.txtRefxR.get()))
			except:
				xRefR = ctypes.c_float(0.0)
			yRefR = ctypes.c_float(0.0)
			try:
				yRefR = ctypes.c_float(float(self.txtRefyR.get()))
			except:
				yRefR = ctypes.c_float(0.0)
			AbsIntRefR = ctypes.c_double(0.0)
			try:
				AbsIntRefR = ctypes.c_double(float(self.txtRefAbsIntR.get()))
			except:
				AbsIntRefR = ctypes.c_double(0.0)
			self.txtLog.insert(tk.END, '\r\n' + 'xRef=' + str(xRefR.value) + ', yRef=' + str(yRefR.value) + ', AbsIntRef=' + str(AbsIntRefR.value))
			
			xRefG = ctypes.c_float(0.0)
			try:
				xRefG = ctypes.c_float(float(self.txtRefxG.get()))
			except:
				xRefG = ctypes.c_float(0.0)
			yRefG = ctypes.c_float(0.0)
			try:
				yRefG = ctypes.c_float(float(self.txtRefyG.get()))
			except:
				yRefG = ctypes.c_float(0.0)
			AbsIntRefG = ctypes.c_double(0.0)
			try:
				AbsIntRefG = ctypes.c_double(float(self.txtRefAbsIntG.get()))
			except:
				AbsIntRefG = ctypes.c_double(0.0)
			self.txtLog.insert(tk.END, '\r\n' + 'xRef=' + str(xRefG.value) + ', yRef=' + str(yRefG.value) + ', AbsIntRef=' + str(AbsIntRefG.value))
			
			xRefB = ctypes.c_float(0.0)
			try:
				xRefB = ctypes.c_float(float(self.txtRefxB.get()))
			except:
				xRefB = ctypes.c_float(0.0)
			yRefB = ctypes.c_float(0.0)
			try:
				yRefB = ctypes.c_float(float(self.txtRefyB.get()))
			except:
				yRefB = ctypes.c_float(0.0)
			AbsIntRefB = ctypes.c_double(0.0)
			try:
				AbsIntRefB = ctypes.c_double(float(self.txtRefAbsIntB.get()))
			except:
				AbsIntRefB = ctypes.c_double(0.0)
			self.txtLog.insert(tk.END, '\r\n' + 'xRef=' + str(xRefB.value) + ', yRef=' + str(yRefB.value) + ', AbsIntRef=' + str(AbsIntRefB.value))
			
			# Reset adjustment
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_ResetRGBAdj(DevPath, f)

			COLORS = ['RED', 'GREEN', 'BLUE']
			for c in COLORS:
				messagebox.showinfo(c, 'Please, switch on ' + c + ' LED and click OK to continue')
				
				# Capture
				self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

				# Store actual measurements
				for f in range(1, NFIBERS + 1):
					ret = self.FeasaCom_UserCal_TakeRGBCurrentValues(DevPath, f, c.encode('ascii')[0])
					if ret != 1:
						self.FeasaCom_GetError_Description(buffer);
						messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
						self.FeasaCom_Close(DevPath)
						return

			# Adjustment
			for f in range(1, NFIBERS + 1):
				ret = self.FeasaCom_UserCal_AdjustRGB(DevPath, f, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB)
				if ret != 1:
					self.FeasaCom_GetError_Description(buffer);
					messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
					self.FeasaCom_Close(DevPath)
					return

			# Capture
			self.FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);
			
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETxyALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\r\n' + 'Results:' + '\r\n' + buffer.value.decode('ascii'))
			
			# Send command to the LED Analyser
			# You can notice that there is no need to send the CR + LF characters (The command Send does it for you automatically)
			ret = self.FeasaCom_Send(DevPath, b"GETABSINTALL", buffer)
			if ret != 1:
				self.FeasaCom_GetError_Description(buffer);
				messagebox.showwarning('Error', 'Error: ' + buffer.value.decode('ascii'))
				self.FeasaCom_Close(DevPath)
				return
			
			self.txtLog.insert(tk.END, '\r\n' + 'Results:' + '\r\n' + buffer.value.decode('ascii'))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')

	def btnReadParams_Click(self):
	
		# A buffer is needed to store the responses obtained from the LED Analyser.
		# This buffer shuld be big enought to fit all the information returned
		BUFFER_SIZE = 1024
		buffer = ctypes.create_string_buffer(BUFFER_SIZE)

		# check if port has been selected
		if self.lstPorts.current() == -1:
			messagebox.showinfo('Select port', 'Please, select a port first')
			return

		# change maximum timeout to avoid timeout events due to long capture (manual/PWM)
		self.FeasaCom_SetResponseTimeout(8000) # 8000 milliseconds

		# Clear results box
		self.txtLog.delete(1.0, tk.END)

		# get settings
		NFIBERS = int(self.numFibers.get())
		DevPath =( '/dev/' + self.lstPorts.get()).encode('ascii')

		# open port
		ret = self.FeasaCom_Open(DevPath, 57600)
		if ret == 1:
			# port opened successfully

			# Retrieve Intensity gains
			Gain = (ctypes.c_int)()
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_GetIntensityGain(DevPath, f, Gain)
				self.txtLog.insert(tk.END, '\n' + 'Int Gain %.2d'%f + ': ' + str(Gain.value))
				
			# Retrieve xy Offsets
			xOffset = (ctypes.c_float)()
			yOffset = (ctypes.c_float)()
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_GetxyOffsets(DevPath, f, xOffset, yOffset)
				self.txtLog.insert(tk.END, '\n' + 'xy Offsets %.2d'%f + ': ' + '%0.4f'%xOffset.value+ '; ' + '%0.4f'%yOffset.value)
				
			# Retrieve Wavelength Offsets
			WavelengthOffset = (ctypes.c_int)()
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_GetWavelengthOffset(DevPath, f, WavelengthOffset)
				self.txtLog.insert(tk.END, '\n' + 'Wl Offsets %.2d'%f + ': ' + str(WavelengthOffset.value))
				
			# Retrieve Abs Int Factor
			AbsIntFactor = (ctypes.c_double)()
			for f in range(1, NFIBERS + 1):
				self.FeasaCom_UserCal_GetAbsIntFactor(DevPath, f, AbsIntFactor)
				self.txtLog.insert(tk.END, '\n' + 'Abs Int Factor %.2d'%f + ': ' + str(AbsIntFactor.value))
			
			# close the port
			self.FeasaCom_Close(DevPath)
		else:
			# unable to open port
			messagebox.showwarning('Error', 'Unable to open the port')
		

app = Application()
app.master.title('Capture modes - (c) Feasa Enterprises Ltd')
app.mainloop()

