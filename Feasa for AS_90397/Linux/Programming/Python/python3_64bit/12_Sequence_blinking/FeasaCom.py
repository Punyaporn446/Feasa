import ctypes
from ctypes import *

#Note: libfeasacom.so file has to be used for 32bit targets

FeasaLIB = cdll.LoadLibrary('libfeasacom.x86_64.so')

# Basic Comm functions
Open = FeasaLIB['FeasaCom_Open']
Open.argtypes = [ctypes.c_char_p, ctypes.c_int]
Open.restype = ctypes.c_int

Send = FeasaLIB['FeasaCom_Send']
Send.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
Send.restype = ctypes.c_int

Close = FeasaLIB['FeasaCom_Close']
Close.argtypes = [ctypes.c_char_p]
Close.restype = ctypes.c_int

OpenSN = FeasaLIB['FeasaCom_OpenSN']
OpenSN.argtypes = [ctypes.c_char_p, ctypes.c_int]
OpenSN.restype = ctypes.c_int

SendSN = FeasaLIB['FeasaCom_SendSN']
SendSN.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
SendSN.restype = ctypes.c_int

CloseSN = FeasaLIB['FeasaCom_CloseSN']
CloseSN.argtypes = [ctypes.c_char_p]
CloseSN.restype = ctypes.c_int


# Comm helper functions
SendToAll = FeasaLIB['FeasaCom_SendToAll']
SendToAll.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
SendToAll.restype = ctypes.c_int

SendToAll_NR = FeasaLIB['FeasaCom_SendToAll_NR']
SendToAll_NR.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p]
SendToAll_NR.restype = ctypes.c_int

Open_Multi = FeasaLIB['FeasaCom_Open_Multi']
Open_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.c_int]
Open_Multi.restype = ctypes.c_int

Close_Multi = FeasaLIB['FeasaCom_Close_Multi']
Close_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
Close_Multi.restype = ctypes.c_int

Send_Multi = FeasaLIB['FeasaCom_Send_Multi']
Send_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]
Send_Multi.restype = ctypes.c_int

Send_Multi_NR = FeasaLIB['FeasaCom_Send_Multi_NR']
Send_Multi_NR.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_char]
Send_Multi_NR.restype = ctypes.c_int

OpenSN_Multi = FeasaLIB['FeasaCom_OpenSN_Multi']
OpenSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.c_int]
OpenSN_Multi.restype = ctypes.c_int

CloseSN_Multi = FeasaLIB['FeasaCom_CloseSN_Multi']
CloseSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
CloseSN_Multi.restype = ctypes.c_int

SendSN_Multi = FeasaLIB['FeasaCom_SendSN_Multi']
SendSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]
SendSN_Multi.restype = ctypes.c_int

CloseAll = FeasaLIB['FeasaCom_CloseAll']
CloseAll.restype = ctypes.c_int

GetResponseByPort = FeasaLIB['FeasaCom_GetResponseByPort']
GetResponseByPort.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
GetResponseByPort.restype = ctypes.c_int

GetOpenedPorts = FeasaLIB['FeasaCom_GetOpenedPorts']
GetOpenedPorts.argtypes = [ctypes.POINTER(ctypes.c_char_p)]
GetOpenedPorts.restype = ctypes.c_int

GetOpenedPortsS = FeasaLIB['FeasaCom_GetOpenedPortsS']
GetOpenedPortsS.argtypes = [ctypes.c_char_p, ctypes.c_char]
GetOpenedPortsS.restype = ctypes.c_int


# Test functions
Capture = FeasaLIB['FeasaCom_Capture']
Capture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
Capture.restype = ctypes.c_int
                
CaptureFromAll = FeasaLIB['FeasaCom_CaptureFromAll']
CaptureFromAll.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_int, ctypes.c_int]
CaptureFromAll.restype = ctypes.c_int

SpectrometerCapture = FeasaLIB['FeasaCom_SpectrometerCapture']
SpectrometerCapture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_float]
SpectrometerCapture.restype = ctypes.c_int
                
CaptureFromAll = FeasaLIB['FeasaCom_CaptureFromAllSpectrometers']
CaptureFromAll.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_int, ctypes.c_float]
CaptureFromAll.restype = ctypes.c_int
                
Sequence_Setup = FeasaLIB['FeasaCom_Sequence_Setup']
Sequence_Setup.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
Sequence_Setup.restype = ctypes.c_int

Sequence_Capture = FeasaLIB['FeasaCom_Sequence_Capture']
Sequence_Capture.argtypes = [ctypes.c_char_p, ctypes.c_int]
Sequence_Capture.restype = ctypes.c_int

Sequence_ReadIntensity = FeasaLIB['FeasaCom_Sequence_ReadIntensity']
Sequence_ReadIntensity.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
Sequence_ReadIntensity.restype = ctypes.c_int

Sequence_ReadxyI = FeasaLIB['FeasaCom_Sequence_ReadxyI']
Sequence_ReadxyI.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadxyI.restype = ctypes.c_int

Sequence_ReadHSI = FeasaLIB['FeasaCom_Sequence_ReadHSI']
Sequence_ReadHSI.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadHSI.restype = ctypes.c_int

Sequence_ReadRGBI = FeasaLIB['FeasaCom_Sequence_ReadRGBI']
Sequence_ReadRGBI.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadRGBI.restype = ctypes.c_int

Sequence_GetPattern = FeasaLIB['FeasaCom_Sequence_GetPattern']
Sequence_GetPattern.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_GetPattern.restype = ctypes.c_int

Sequence_GetSweepingPattern = FeasaLIB['FeasaCom_Sequence_GetSweepingPattern']
Sequence_GetSweepingPattern.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_GetSweepingPattern.restype = ctypes.c_int

Sequence_GetFrequency = FeasaLIB['FeasaCom_Sequence_GetFrequency']
Sequence_GetFrequency.argtypes = [ctypes.c_char_p, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Sequence_GetFrequency.restype = ctypes.c_int

Sequence_FindTestSettings = FeasaLIB['FeasaCom_Sequence_FindTestSettings']
Sequence_FindTestSettings.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_FindTestSettings.restype = ctypes.c_int

Sequence_SetPatternThresholdHigh = FeasaLIB['FeasaCom_Sequence_SetPatternThresholdHigh']
Sequence_SetPatternThresholdHigh.argtypes = [ctypes.c_char_p, ctypes.c_int]
Sequence_SetPatternThresholdHigh.restype = ctypes.c_int

Sequence_SetPatternThresholdLow = FeasaLIB['FeasaCom_Sequence_SetPatternThresholdLow']
Sequence_SetPatternThresholdLow.argtypes = [ctypes.c_char_p, ctypes.c_int]
Sequence_SetPatternThresholdLow.restype = ctypes.c_int


# Daisy-chain functions
DaisyChain_Add = FeasaLIB['FeasaCom_DaisyChain_Add']
DaisyChain_Add.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
DaisyChain_Add.restype = ctypes.c_int

DaisyChain_Del = FeasaLIB['FeasaCom_DaisyChain_Del']
DaisyChain_Del.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
DaisyChain_Del.restype = ctypes.c_int

DaisyChain_Clear = FeasaLIB['FeasaCom_DaisyChain_Clear']
DaisyChain_Clear.argtypes = [ctypes.c_char_p]
DaisyChain_Clear.restype = ctypes.c_int

DaisyChain_Capture = FeasaLIB['FeasaCom_DaisyChain_Capture']
DaisyChain_Capture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
DaisyChain_Capture.restype = ctypes.c_int

DaisyChain_SpectrometerCapture = FeasaLIB['FeasaCom_DaisyChain_SpectrometerCapture']
DaisyChain_SpectrometerCapture.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_float]
DaisyChain_SpectrometerCapture.restype = ctypes.c_int


# External Trigger functions
ExternalTrigger_Listen = FeasaLIB['FeasaCom_ExternalTrigger_Listen']
ExternalTrigger_Listen.argtypes = [ctypes.c_char_p]
ExternalTrigger_Listen.restype = ctypes.c_int

ExternalTrigger_Abort = FeasaLIB['FeasaCom_ExternalTrigger_Abort']
ExternalTrigger_Abort.argtypes = [ctypes.c_char_p]
ExternalTrigger_Abort.restype = ctypes.c_int

ExternalTrigger_isFinished = FeasaLIB['FeasaCom_ExternalTrigger_isFinished']
ExternalTrigger_isFinished.argtypes = [ctypes.c_char_p]
ExternalTrigger_isFinished.restype = ctypes.c_int


# Comm handling functions
EnumPorts = FeasaLIB['FeasaCom_EnumPorts']
EnumPorts.restype = ctypes.c_int

IsConnected = FeasaLIB['FeasaCom_IsConnected']
IsConnected.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int]
IsConnected.restype = ctypes.c_int

AreConnected = FeasaLIB['FeasaCom_AreConnected']
AreConnected.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.c_int]
AreConnected.restype = ctypes.c_int

AreConnectedS = FeasaLIB['FeasaCom_AreConnectedS']
AreConnectedS.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int]
AreConnectedS.restype = ctypes.c_int

Detect = FeasaLIB['FeasaCom_Detect']
Detect.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
Detect.restype = ctypes.c_int

DetectS = FeasaLIB['FeasaCom_DetectS']
DetectS.argtypes = [ctypes.c_char_p, ctypes.c_char, ctypes.c_int]
DetectS.restype = ctypes.c_int

DetectSN = FeasaLIB['FeasaCom_DetectSN']
DetectSN.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
DetectSN.restype = ctypes.c_int

AddDetectionFilter = FeasaLIB['FeasaCom_AddDetectionFilter']
AddDetectionFilter.argtypes = [ctypes.c_char_p]

ClearDetectionFilters = FeasaLIB['FeasaCom_ClearDetectionFilters']

IsPortAvailable = FeasaLIB['FeasaCom_IsPortAvailable']
IsPortAvailable.argtypes = [ctypes.c_char_p]
IsPortAvailable.restype = ctypes.c_int

ListPortsDetected = FeasaLIB['FeasaCom_ListPortsDetected']
ListPortsDetected.argtypes = [POINTER(ctypes.c_char_p)]
ListPortsDetected.restype = ctypes.c_int

ListPortsDetectedTxt = FeasaLIB['FeasaCom_ListPortsDetectedTxt']
ListPortsDetectedTxt.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
ListPortsDetectedTxt.restype = ctypes.c_int

SetResponseTimeout = FeasaLIB['FeasaCom_SetResponseTimeout']
SetResponseTimeout.argtypes = [ctypes.c_uint]
SetResponseTimeout.restype = ctypes.c_int

GetBaudrate = FeasaLIB['FeasaCom_GetBaudrate']
GetBaudrate.argtypes = [ctypes.c_char_p]
GetBaudrate.restype = ctypes.c_long

GetError_Description = FeasaLIB['FeasaCom_GetError_Description']
GetError_Description.argtypes = [ctypes.c_char_p]

GetPortBySN = FeasaLIB['FeasaCom_GetPortBySN']
GetPortBySN.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
GetPortBySN.restype = ctypes.c_int

GetSNByPort = FeasaLIB['FeasaCom_GetSNByPort']
GetSNByPort.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
GetSNByPort.restype = ctypes.c_int


# Binning
Binning_GetBinFromVECFile = FeasaLIB['FeasaCom_Binning_GetBinFromVECFile']
Binning_GetBinFromVECFile.argtypes = [ctypes.c_char_p, ctypes.c_float, ctypes.c_float, ctypes.c_char_p]
Binning_GetBinFromVECFile.restype = ctypes.c_int
		

# UserCal functions
UserCal_ResetIntensity = FeasaLIB['FeasaCom_UserCal_ResetIntensity']
UserCal_ResetIntensity.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
UserCal_ResetIntensity.restype = ctypes.c_int

UserCal_GetIntensityGain = FeasaLIB['FeasaCom_UserCal_GetIntensityGain']
UserCal_GetIntensityGain.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
UserCal_GetIntensityGain.restype = ctypes.c_int

UserCal_SetIntensityGain = FeasaLIB['FeasaCom_UserCal_SetIntensityGain']
UserCal_SetIntensityGain.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_SetIntensityGain.restype = ctypes.c_int

UserCal_AdjustIntensity = FeasaLIB['FeasaCom_UserCal_AdjustIntensity']
UserCal_AdjustIntensity.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_AdjustIntensity.restype = ctypes.c_int


UserCal_ResetAbsInt = FeasaLIB['FeasaCom_UserCal_ResetAbsInt']
UserCal_ResetAbsInt.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
UserCal_ResetAbsInt.restype = ctypes.c_int

UserCal_GetAbsIntFactor = FeasaLIB['FeasaCom_UserCal_GetAbsIntFactor']
UserCal_GetAbsIntFactor.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_double)]
UserCal_GetAbsIntFactor.restype = ctypes.c_int

UserCal_SetAbsIntFactor = FeasaLIB['FeasaCom_UserCal_SetAbsIntFactor']
UserCal_SetAbsIntFactor.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_double, ctypes.c_int]
UserCal_SetAbsIntFactor.restype = ctypes.c_int

UserCal_AdjustAbsInt = FeasaLIB['FeasaCom_UserCal_AdjustAbsInt']
UserCal_AdjustAbsInt.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_double, ctypes.c_int]
UserCal_AdjustAbsInt.restype = ctypes.c_int


UserCal_ResetWavelengthOffset = FeasaLIB['FeasaCom_UserCal_ResetWavelengthOffset']
UserCal_ResetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
UserCal_ResetWavelengthOffset.restype = ctypes.c_int

UserCal_GetWavelengthOffset = FeasaLIB['FeasaCom_UserCal_GetWavelengthOffset']
UserCal_GetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
UserCal_GetWavelengthOffset.restype = ctypes.c_int

UserCal_SetWavelengthOffset = FeasaLIB['FeasaCom_UserCal_SetWavelengthOffset']
UserCal_SetWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_SetWavelengthOffset.restype = ctypes.c_int

UserCal_AdjustWavelengthOffset = FeasaLIB['FeasaCom_UserCal_AdjustWavelengthOffset']
UserCal_AdjustWavelengthOffset.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_AdjustWavelengthOffset.restype = ctypes.c_int


UserCal_ResetxyOffsets = FeasaLIB['FeasaCom_UserCal_ResetxyOffsets']
UserCal_ResetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_int]
UserCal_ResetxyOffsets.restype = ctypes.c_int

UserCal_GetxyOffsets = FeasaLIB['FeasaCom_UserCal_GetxyOffsets']
UserCal_GetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
UserCal_GetxyOffsets.restype = ctypes.c_int

UserCal_SetxyOffsets = FeasaLIB['FeasaCom_UserCal_SetxyOffsets']
UserCal_SetxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
UserCal_SetxyOffsets.restype = ctypes.c_int

UserCal_AdjustxyOffsets = FeasaLIB['FeasaCom_UserCal_AdjustxyOffsets']
UserCal_AdjustxyOffsets.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
UserCal_AdjustxyOffsets.restype = ctypes.c_int


UserCal_ResetRGBAdj = FeasaLIB['FeasaCom_UserCal_ResetRGBAdj']
UserCal_ResetRGBAdj.argtypes = [ctypes.c_char_p, ctypes.c_int]
UserCal_ResetRGBAdj.restype = ctypes.c_int

UserCal_TakeRGBCurrentValues = FeasaLIB['FeasaCom_UserCal_TakeRGBCurrentValues']
UserCal_TakeRGBCurrentValues.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_char]
UserCal_TakeRGBCurrentValues.restype = ctypes.c_int

UserCal_AdjustRGB = FeasaLIB['FeasaCom_UserCal_AdjustRGB']
UserCal_AdjustRGB.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double]
UserCal_AdjustRGB.restype = ctypes.c_int


def CreateCArrayOfInt(ArraySize, defValue):
	Variable = (ctypes.c_int * ArraySize)()
	Pointer = (ctypes.c_int * ArraySize)(*Variable)
	for i in range(ArraySize):
		Variable[i] = defValue
	return [Variable, Pointer]

def CreateCArrayOfStrings(ArraySize, ptringLength):
	Variable = [ctypes.create_string_buffer(ptringLength) for i in range(ArraySize)]
	Pointer = (ctypes.c_char_p*ArraySize)(*map(ctypes.addressof, Variable))
	for i in range(ArraySize):
		Variable[i].value = b""
	return [Variable, Pointer]

	