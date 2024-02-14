#********************************************************
#  Copyright 2020 Feasa Enterprises Ltd
#  Feasa Communications Library
#********************************************************

import ctypes

#Note: feasacom.dll file has to be used for 32bit targets

FeasaDLL = ctypes.WinDLL('feasacom64.dll')

GetDLLVersion = FeasaDLL['FeasaCom_GetDLLVersion']
GetDLLVersion.argtypes = [ctypes.c_char_p]

# Basic Comm functions
Open = FeasaDLL['FeasaCom_Open']
Open.argtypes = [ctypes.c_int, ctypes.c_char_p]
Open.restype = ctypes.c_int

Close = FeasaDLL['FeasaCom_Close']
Close.argtypes = [ctypes.c_int]
Close.restype = ctypes.c_int

Send = FeasaDLL['FeasaCom_Send']
Send.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_char_p]
Send.restype = ctypes.c_int

OpenSN = FeasaDLL['FeasaCom_OpenSN']
OpenSN.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
OpenSN.restype = ctypes.c_int

CloseSN = FeasaDLL['FeasaCom_CloseSN']
CloseSN.argtypes = [ctypes.c_char_p]
CloseSN.restype = ctypes.c_int

SendSN = FeasaDLL['FeasaCom_SendSN']
SendSN.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
SendSN.restype = ctypes.c_int


# Comm helper functions
SendToAll = FeasaDLL['FeasaCom_SendToAll']
SendToAll.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p, ctypes.POINTER(ctypes.c_char_p)]
SendToAll.restype = ctypes.c_int

SendToAll_NR = FeasaDLL['FeasaCom_SendToAll_NR']
SendToAll_NR.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p]
SendToAll_NR.restype = ctypes.c_int

Open_Multi = FeasaDLL['FeasaCom_Open_Multi']
Open_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_char_p]
Open_Multi.restype = ctypes.c_int

Close_Multi = FeasaDLL['FeasaCom_Close_Multi']
Close_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.c_int]
Close_Multi.restype = ctypes.c_int

Send_Multi = FeasaDLL['FeasaCom_Send_Multi']
Send_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]
Send_Multi.restype = ctypes.c_int

Send_Multi_NR = FeasaDLL['FeasaCom_Send_Multi_NR']
Send_Multi_NR.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_char_p, ctypes.c_char]
Send_Multi_NR.restype = ctypes.c_int

OpenSN_Multi = FeasaDLL['FeasaCom_OpenSN_Multi']
OpenSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.c_char_p]
OpenSN_Multi.restype = ctypes.c_int

CloseSN_Multi = FeasaDLL['FeasaCom_CloseSN_Multi']
CloseSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int]
CloseSN_Multi.restype = ctypes.c_int

SendSN_Multi = FeasaDLL['FeasaCom_SendSN_Multi']
SendSN_Multi.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)]
SendSN_Multi.restype = ctypes.c_int

CloseAll = FeasaDLL['FeasaCom_CloseAll']
CloseAll.restype = ctypes.c_int

GetResponseByPort = FeasaDLL['FeasaCom_GetResponseByPort']
GetResponseByPort.argtypes = [ctypes.c_int, ctypes.c_char_p]
GetResponseByPort.restype = ctypes.c_int


# Test functions
Capture = FeasaDLL['FeasaCom_Capture']
Capture.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
Capture.restype = ctypes.c_int

CaptureFromAll = FeasaDLL['FeasaCom_CaptureFromAll']
CaptureFromAll.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_int, ctypes.c_int]
CaptureFromAll.restype = ctypes.c_int

SpectrometerCapture = FeasaDLL['FeasaCom_SpectrometerCapture']
SpectrometerCapture.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_float]
SpectrometerCapture.restype = ctypes.c_int

SpectrometerDark = FeasaDLL['FeasaCom_SpectrometerDark']
SpectrometerDark.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_float]
SpectrometerDark.restype = ctypes.c_int

CaptureFromAllSpectrometers = FeasaDLL['FeasaCom_CaptureFromAllSpectrometers']
CaptureFromAllSpectrometers.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.c_int, ctypes.c_float]
CaptureFromAllSpectrometers.restype = ctypes.c_int

Sequence_Setup = FeasaDLL['FeasaCom_Sequence_Setup']
Sequence_Setup.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
Sequence_Setup.restype = ctypes.c_int

Sequence_Capture = FeasaDLL['FeasaCom_Sequence_Capture']
Sequence_Capture.argtypes = [ctypes.c_int, ctypes.c_int]
Sequence_Capture.restype = ctypes.c_int

Sequence_ReadIntensity = FeasaDLL['FeasaCom_Sequence_ReadIntensity']
Sequence_ReadIntensity.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
Sequence_ReadIntensity.restype = ctypes.c_int

Sequence_ReadxyI = FeasaDLL['FeasaCom_Sequence_ReadxyI']
Sequence_ReadxyI.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadxyI.restype = ctypes.c_int

Sequence_ReadHSI = FeasaDLL['FeasaCom_Sequence_ReadHSI']
Sequence_ReadHSI.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadHSI.restype = ctypes.c_int

Sequence_ReadRGBI = FeasaDLL['FeasaCom_Sequence_ReadRGBI']
Sequence_ReadRGBI.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_ubyte), ctypes.POINTER(ctypes.c_int)]
Sequence_ReadRGBI.restype = ctypes.c_int

Sequence_ReadCCT = FeasaDLL['FeasaCom_Sequence_ReadCCT']
Sequence_ReadCCT.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_float)]
Sequence_ReadCCT.restype = ctypes.c_int

Sequence_ReadWavelength = FeasaDLL['FeasaCom_Sequence_ReadWavelength']
Sequence_ReadWavelength.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
Sequence_ReadWavelength.restype = ctypes.c_int

Sequence_GetPattern = FeasaDLL['FeasaCom_Sequence_GetPattern']
Sequence_GetPattern.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_GetPattern.restype = ctypes.c_int

Sequence_GetSweepingPattern = FeasaDLL['FeasaCom_Sequence_GetSweepingPattern']
Sequence_GetSweepingPattern.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_GetSweepingPattern.restype = ctypes.c_int

Sequence_GetFrequency = FeasaDLL['FeasaCom_Sequence_GetFrequency']
Sequence_GetFrequency.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_int)]
Sequence_GetFrequency.restype = ctypes.c_int

Sequence_FindTestSettings = FeasaDLL['FeasaCom_Sequence_FindTestSettings']
Sequence_FindTestSettings.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
Sequence_FindTestSettings.restype = ctypes.c_int

Sequence_SetPatternThresholdHigh = FeasaDLL['FeasaCom_Sequence_SetPatternThresholdHigh']
Sequence_SetPatternThresholdHigh.argtypes = [ctypes.c_int, ctypes.c_int]
Sequence_SetPatternThresholdHigh.restype = ctypes.c_int

Sequence_SetPatternThresholdLow = FeasaDLL['FeasaCom_Sequence_SetPatternThresholdLow']
Sequence_SetPatternThresholdLow.argtypes = [ctypes.c_int, ctypes.c_int]
Sequence_SetPatternThresholdLow.restype = ctypes.c_int


# Daisy-chain functions
DaisyChain_Add = FeasaDLL['FeasaCom_DaisyChain_Add']
DaisyChain_Add.argtypes = [ctypes.c_int, ctypes.c_char_p]
DaisyChain_Add.restype = ctypes.c_int

DaisyChain_Del = FeasaDLL['FeasaCom_DaisyChain_Del']
DaisyChain_Del.argtypes = [ctypes.c_int, ctypes.c_char_p]
DaisyChain_Del.restype = ctypes.c_int

DaisyChain_Clear = FeasaDLL['FeasaCom_DaisyChain_Clear']
DaisyChain_Clear.argtypes = [ctypes.c_int]
DaisyChain_Clear.restype = ctypes.c_int

DaisyChain_Send = FeasaDLL['FeasaCom_DaisyChain_Send']
DaisyChain_Send.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
DaisyChain_Send.restype = ctypes.c_int

DaisyChain_Capture = FeasaDLL['FeasaCom_DaisyChain_Capture']
DaisyChain_Capture.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
DaisyChain_Capture.restype = ctypes.c_int

DaisyChain_SpectrometerCapture = FeasaDLL['FeasaCom_DaisyChain_SpectrometerCapture']
DaisyChain_SpectrometerCapture.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_float]
DaisyChain_SpectrometerCapture.restype = ctypes.c_int

DaisyChain_SpectrometerDark = FeasaDLL['FeasaCom_DaisyChain_SpectrometerDark']
DaisyChain_SpectrometerDark.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_float]
DaisyChain_SpectrometerDark.restype = ctypes.c_int


# External Trigger functions
ExternalTrigger_Listen = FeasaDLL['FeasaCom_ExternalTrigger_Listen']
ExternalTrigger_Listen.argtypes = [ctypes.c_int]
ExternalTrigger_Listen.restype = ctypes.c_int

ExternalTrigger_Abort = FeasaDLL['FeasaCom_ExternalTrigger_Abort']
ExternalTrigger_Abort.argtypes = [ctypes.c_int]
ExternalTrigger_Abort.restype = ctypes.c_int

ExternalTrigger_isFinished = FeasaDLL['FeasaCom_ExternalTrigger_isFinished']
ExternalTrigger_isFinished.argtypes = [ctypes.c_int]
ExternalTrigger_isFinished.restype = ctypes.c_int


# Comm handling functions
EnumPorts = FeasaDLL['FeasaCom_EnumPorts']
EnumPorts.restype = ctypes.c_int

EnumPorts_Filter = FeasaDLL['FeasaCom_EnumPorts_Filter']
EnumPorts_Filter.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int]

IsConnected = FeasaDLL['FeasaCom_IsConnected']
IsConnected.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
IsConnected.restype = ctypes.c_int

AreConnected = FeasaDLL['FeasaCom_AreConnected']
AreConnected.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_char_p), ctypes.c_int, ctypes.c_char_p]
AreConnected.restype = ctypes.c_int

AreConnectedS = FeasaDLL['FeasaCom_AreConnectedS']
AreConnectedS.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p, ctypes.c_char_p]
AreConnectedS.restype = ctypes.c_int

Detect = FeasaDLL['FeasaCom_Detect']
Detect.argtypes = [ctypes.POINTER(ctypes.c_int), ctypes.c_char_p]
Detect.restype = ctypes.c_int

DetectSN = FeasaDLL['FeasaCom_DetectSN']
DetectSN.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_char_p]
DetectSN.restype = ctypes.c_int

AddDetectionFilter = FeasaDLL['FeasaCom_AddDetectionFilter']
AddDetectionFilter.argtypes = [ctypes.c_char_p]

ClearDetectionFilters = FeasaDLL['FeasaCom_ClearDetectionFilters']

IsPortAvailable = FeasaDLL['FeasaCom_IsPortAvailable']
IsPortAvailable.argtypes = [ctypes.c_int]
IsPortAvailable.restype = ctypes.c_int

ListPortsDetected = FeasaDLL['FeasaCom_ListPortsDetected']
ListPortsDetected.argtypes = [ctypes.POINTER(ctypes.c_int)]
ListPortsDetected.restype = ctypes.c_int

ListPortsDetectedTxt = FeasaDLL['FeasaCom_ListPortsDetectedTxt']
ListPortsDetectedTxt.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
ListPortsDetectedTxt.restype = ctypes.c_int

SetResponseTimeout = FeasaDLL['FeasaCom_SetResponseTimeout']
SetResponseTimeout.argtypes = [ctypes.c_uint]
SetResponseTimeout.restype = ctypes.c_int

SetResponseTimeoutAuto = FeasaDLL['FeasaCom_SetResponseTimeoutAuto']
SetResponseTimeoutAuto.argtypes = [ctypes.c_int, ctypes.c_int]
SetResponseTimeoutAuto.restype = ctypes.c_int

GetBaudrate = FeasaDLL['FeasaCom_GetBaudrate']
GetBaudrate.argtypes = [ctypes.c_int]
GetBaudrate.restype = ctypes.c_long

GetDeviceType = FeasaDLL['FeasaCom_GetDeviceType']
GetDeviceType.argtypes = [ctypes.c_int, ctypes.c_char_p]
GetDeviceType.restype = ctypes.c_int

GetError_Description = FeasaDLL['FeasaCom_GetError_Description']
GetError_Description.argtypes = [ctypes.c_char_p]

GetError_DescriptionByPort = FeasaDLL['FeasaCom_GetError_DescriptionByPort']
GetError_DescriptionByPort.argtypes = [ctypes.c_int, ctypes.c_char_p]

GetError_DescriptionBySN = FeasaDLL['FeasaCom_GetError_DescriptionBySN']
GetError_DescriptionBySN.argtypes = [ctypes.c_char_p, ctypes.c_char_p]

GetPortBySN = FeasaDLL['FeasaCom_GetPortBySN']
GetPortBySN.argtypes = [ctypes.c_char_p]
GetPortBySN.restype = ctypes.c_int

GetSNByPort = FeasaDLL['FeasaCom_GetSNByPort']
GetSNByPort.argtypes = [ctypes.c_char_p, ctypes.c_int]
GetSNByPort.restype = ctypes.c_int

GetPortByID = FeasaDLL['FeasaCom_GetPortByID']
GetPortByID.argtypes = [ctypes.c_char_p]
GetPortByID.restype = ctypes.c_int

GetOpenedPorts = FeasaDLL['FeasaCom_GetOpenedPorts']
GetOpenedPorts.argtypes = [ctypes.POINTER(ctypes.c_int)]
GetOpenedPorts.restype = ctypes.c_int

GetOpenedPortsS = FeasaDLL['FeasaCom_GetOpenedPortsS']
GetOpenedPortsS.argtypes = [ctypes.c_char_p, ctypes.c_char]
GetOpenedPortsS.restype = ctypes.c_int

OpenProject = FeasaDLL['FeasaCom_OpenProject']
OpenProject.argtypes = [ctypes.c_char_p]
OpenProject.restype = ctypes.c_int

CloseProject = FeasaDLL['FeasaCom_CloseProject']
CloseProject.restype = ctypes.c_int

SendByID = FeasaDLL['FeasaCom_SendByID']
SendByID.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
SendByID.restype = ctypes.c_int


# Binning
Binning_GetBinFromVECFile = FeasaDLL['FeasaCom_Binning_GetBinFromVECFile']
Binning_GetBinFromVECFile.argtypes = [ctypes.c_char_p, ctypes.c_float, ctypes.c_float, ctypes.c_char_p]
Binning_GetBinFromVECFile.restype = ctypes.c_int


# UserCal functions
UserCal_ResetIntensity = FeasaDLL['FeasaCom_UserCal_ResetIntensity']
UserCal_ResetIntensity.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_ResetIntensity.restype = ctypes.c_int

UserCal_GetIntensityGain = FeasaDLL['FeasaCom_UserCal_GetIntensityGain']
UserCal_GetIntensityGain.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
UserCal_GetIntensityGain.restype = ctypes.c_int

UserCal_SetIntensityGain = FeasaDLL['FeasaCom_UserCal_SetIntensityGain']
UserCal_SetIntensityGain.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_SetIntensityGain.restype = ctypes.c_int

UserCal_AdjustIntensity = FeasaDLL['FeasaCom_UserCal_AdjustIntensity']
UserCal_AdjustIntensity.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_AdjustIntensity.restype = ctypes.c_int


UserCal_ResetAbsInt = FeasaDLL['FeasaCom_UserCal_ResetAbsInt']
UserCal_ResetAbsInt.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_ResetAbsInt.restype = ctypes.c_int

UserCal_GetAbsIntFactor = FeasaDLL['FeasaCom_UserCal_GetAbsIntFactor']
UserCal_GetAbsIntFactor.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_double)]
UserCal_GetAbsIntFactor.restype = ctypes.c_int

UserCal_SetAbsIntFactor = FeasaDLL['FeasaCom_UserCal_SetAbsIntFactor']
UserCal_SetAbsIntFactor.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_double, ctypes.c_int]
UserCal_SetAbsIntFactor.restype = ctypes.c_int

UserCal_AdjustAbsInt = FeasaDLL['FeasaCom_UserCal_AdjustAbsInt']
UserCal_AdjustAbsInt.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_double, ctypes.c_int]
UserCal_AdjustAbsInt.restype = ctypes.c_int


UserCal_ResetWavelengthOffset = FeasaDLL['FeasaCom_UserCal_ResetWavelengthOffset']
UserCal_ResetWavelengthOffset.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_ResetWavelengthOffset.restype = ctypes.c_int

UserCal_GetWavelengthOffset = FeasaDLL['FeasaCom_UserCal_GetWavelengthOffset']
UserCal_GetWavelengthOffset.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int)]
UserCal_GetWavelengthOffset.restype = ctypes.c_int

UserCal_SetWavelengthOffset = FeasaDLL['FeasaCom_UserCal_SetWavelengthOffset']
UserCal_SetWavelengthOffset.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_SetWavelengthOffset.restype = ctypes.c_int

UserCal_AdjustWavelengthOffset = FeasaDLL['FeasaCom_UserCal_AdjustWavelengthOffset']
UserCal_AdjustWavelengthOffset.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_AdjustWavelengthOffset.restype = ctypes.c_int


UserCal_ResetxyOffsets = FeasaDLL['FeasaCom_UserCal_ResetxyOffsets']
UserCal_ResetxyOffsets.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_int]
UserCal_ResetxyOffsets.restype = ctypes.c_int

UserCal_GetxyOffsets = FeasaDLL['FeasaCom_UserCal_GetxyOffsets']
UserCal_GetxyOffsets.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float)]
UserCal_GetxyOffsets.restype = ctypes.c_int

UserCal_SetxyOffsets = FeasaDLL['FeasaCom_UserCal_SetxyOffsets']
UserCal_SetxyOffsets.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
UserCal_SetxyOffsets.restype = ctypes.c_int

UserCal_AdjustxyOffsets = FeasaDLL['FeasaCom_UserCal_AdjustxyOffsets']
UserCal_AdjustxyOffsets.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_int]
UserCal_AdjustxyOffsets.restype = ctypes.c_int


UserCal_ResetRGBAdj = FeasaDLL['FeasaCom_UserCal_ResetRGBAdj']
UserCal_ResetRGBAdj.argtypes = [ctypes.c_int, ctypes.c_int]
UserCal_ResetRGBAdj.restype = ctypes.c_int

UserCal_TakeRGBCurrentValues = FeasaDLL['FeasaCom_UserCal_TakeRGBCurrentValues']
UserCal_TakeRGBCurrentValues.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_char]
UserCal_TakeRGBCurrentValues.restype = ctypes.c_int

UserCal_AdjustRGB = FeasaDLL['FeasaCom_UserCal_AdjustRGB']
UserCal_AdjustRGB.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double, ctypes.c_float, ctypes.c_float, ctypes.c_double]
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

	