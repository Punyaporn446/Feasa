'********************************************************
'  Copyright 2020 Feasa Enterprises Ltd
'  Feasa Communications Library
'********************************************************

Option Strict On
Option Explicit On

Imports System.Runtime.InteropServices
Imports System.Text

Module FeasaCom

    ' Note: feasacom.dll file has to be used for 32bit targets
    Private Const DLL_PATH As String = "feasacom64.dll"

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetDLLVersion")>
    Public Sub GetDLLVersion(
        ByVal Version As StringBuilder
        )
    End Sub

    ' Basic Comm functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Open")>
    Public Function Open(
        ByVal CommPort As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Send")>
    Public Function Send(
        ByVal CommPort As Integer, ByVal Command As String, ByVal ResponseText As StringBuilder
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Close")>
    Public Function Close(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_OpenSN")>
    Public Function OpenSN(
        ByVal SerialNumber As String, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SendSN")>
    Public Function SendSN(
        ByVal SerialNumber As String, ByVal Command As String, ByVal ResponseText As StringBuilder
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CloseSN")>
    Public Function CloseSN(
        ByVal SerialNumber As String
        ) As Integer
    End Function


    ' Comm helper functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SendToAll")>
    Public Function SendToAll(
        ByVal ReturnValues() As Integer, ByVal Command As String, <[In], Out> ByVal Responses() As String
        ) As Integer
    End Function
	
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SendToAll_NR")>
    Public Function SendToAll_NR(
        ByVal ReturnValues() As Integer, ByVal Command As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Open_Multi")>
    Public Function Open_Multi(
        ByVal ReturnValues() As Integer, ByVal CommPorts() As Integer, ByVal nPorts As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Close_Multi")>
    Public Function Close_Multi(
        ByVal ReturnValues() As Integer, ByVal CommPorts() As Integer, ByVal nPorts As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Send_Multi")>
    Public Function Send_Multi(
        ByVal ReturnValues() As Integer, ByVal CommPorts() As Integer, ByVal nPorts As Integer, ByVal Commands() As String, ByVal Responses() As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Send_Multi_NR")>
    Public Function Send_Multi_NR(
        ByVal ReturnValues() As Integer, ByVal CommPorts() As Integer, ByVal nPorts As Integer, ByVal Commands As StringBuilder, ByVal CommandSeparator as Char
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_OpenSN_Multi")>
    Public Function OpenSN_Multi(
        ByVal ReturnValues() As Integer, ByVal SerialNumbers() As String, ByVal nSerials As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CloseSN_Multi")>
    Public Function CloseSN_Multi(
        ByVal ReturnValues() As Integer, ByVal SerialNumbers() As String, ByVal nSerials As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SendSN_Multi")>
    Public Function SendSN_Multi(
        ByVal ReturnValues() As Integer, ByVal SerialNumbers() As String, ByVal nSerials As Integer, ByVal Commands() As String, <[In], Out> ByVal Responses() As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CloseAll")>
    Public Function CloseAll(
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetResponseByPort")> _
    Public Function GetResponseByPort( _
        ByVal CommPort As Integer, ByVal ResponseText As StringBuilder _
        ) As Integer
    End Function


    ' Test functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Capture")>
    Public Function Capture(
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal CapturePWMFrames As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CaptureFromAll")>
    Public Function CaptureFromAll(
        ByVal ReturnValues() As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal CapturePWMFrames As Integer
        ) As Integer
    End Function
	
	<DllImport(DLL_PATH, EntryPoint:="FeasaCom_SpectrometerCapture")> _
    Public Function SpectrometerCapture( _
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal UseCustomExposure As Integer, ByVal ExposureTime As Single _
        ) As Integer
    End Function
	
	<DllImport(DLL_PATH, EntryPoint:="FeasaCom_SpectrometerDark")> _
    Public Function SpectrometerDark( _
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal UseCustomExposure As Integer, ByVal ExposureTime As Single _
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CaptureFromAllSpectrometers")>
    Public Function CaptureFromAllSpectrometers(
        ByVal ReturnValues() As Integer, ByVal isPWM As Integer, ByVal UseCustomExposure As Integer, ByVal ExposureTime As Single
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Sequence_Setup")>
    Public Function Sequence_Setup(
        ByVal CommPort As Integer, ByVal StartDelay As Integer, ByVal CaptureTime As Integer, ByVal TimeBetweenCaptures As Integer, ByVal SampleCount As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Sequence_Capture")>
    Public Function Sequence_Capture(
        ByVal CommPort As Integer, ByVal Fiber As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Sequence_ReadIntensity")>
    Public Function Sequence_ReadIntensity(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Sequence_ReadxyI")>
    Public Function Sequence_ReadxyI(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xValues() As Single, ByVal yValues() As Single, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_ReadHSI")>
    Public Function Sequence_ReadHSI(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal HueValues() As Single, ByVal SaturationValues() As Integer, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_ReadRGBI")>
    Public Function Sequence_ReadRGBI(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal RedValues() As Byte, ByVal GreenValues() As Byte, ByVal BlueValues() As Byte, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_ReadCCT")>
    Public Function Sequence_ReadCCT(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal CCTValues() As Integer, ByVal deltauvValues() As Single
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_ReadWavelength")>
    Public Function Sequence_ReadWavelength(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal WavelengthValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_GetPattern")>
    Public Function Sequence_GetPattern(
        ByVal CommPort As Integer, ByVal IntensityValues() As Integer, ByVal StatusCount As Integer, ByVal PatternTimes() As Integer, ByVal PatternIntensities() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_GetSweepingPattern")>
    Public Function Sequence_GetSweepingPattern(
        ByVal CommPort As Integer, ByVal LEDCount As Integer, ByVal isOffToOn As Integer, ByVal LowTimes() As Integer, ByVal HighTimes() As Integer, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_GetFrequency")>
    Public Function Sequence_GetFrequency(
        ByVal CommPort As Integer, ByVal IntensityValues() As Integer, ByRef Frequency As Single, ByRef DC As Single, ByRef CycleCount As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_FindTestSettings")>
    Public Function Sequence_FindTestSettings(
        ByVal CommPort As Integer, ByVal TotalLEDCount As Integer, ByVal FiberToTest As Integer, ByVal SignalSpeed As Integer, ByVal BlinkingSpeed As Integer, ByVal MinCycleCount As Integer, ByVal TimeResolutionIsImportant As Integer, ByRef CaptureTime As Integer, ByRef WaitTime As Integer, ByRef SampleCount As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_SetPatternThresholdHigh")>
    Public Function Sequence_SetPatternThresholdHigh(
        ByVal CommPort As Integer, ByVal Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Sequence_SetPatternThresholdLow")>
    Public Function Sequence_SetPatternThresholdLow(
        ByVal CommPort As Integer, ByVal Intensity As Integer
        ) As Integer
    End Function



    ' Daisy-chain helper functions
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_Add")>
    Public Function DaisyChain_Add(
        ByVal CommPort As Integer, ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_Del")>
    Public Function DaisyChain_Del(
        ByVal CommPort As Integer, ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_Clear")>
    Public Function DaisyChain_Clear(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_Send")> _
    Public Function DaisyChain_Send( _
        ByVal CommPort As Integer, ByVal SerialNumber As String, ByVal Command As String, ByVal ResponseText As StringBuilder _
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_Capture")>
    Public Function DaisyChain_Capture(
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal CapturePWMFrames As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_SpectrometerCapture")> _
    Public Function DaisyChain_SpectrometerCapture( _
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal UsePresetExposure As Integer, ByVal ExposureTime As Single _
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DaisyChain_SpectrometerDark")> _
    Public Function DaisyChain_SpectrometerDark( _
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal UsePresetExposure As Integer, ByVal ExposureTime As Single _
        ) As Integer
    End Function
	
	
	' External Trigger functions
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ExternalTrigger_Listen")> _
    Public Function ExternalTrigger_Listen( _
        ByVal CommPort As Integer _
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ExternalTrigger_Abort")> _
    Public Function ExternalTrigger_Abort( _
        ByVal CommPort As Integer _
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ExternalTrigger_isFinished")> _
    Public Function ExternalTrigger_isFinished( _
        ByVal CommPort As Integer _
        ) As Integer
    End Function


    ' Comm handling functions
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_EnumPorts")>
    Public Function EnumPorts() As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_EnumPorts_Filter")> _
    Public Sub EnumPorts_Filter( _
        ByVal USB As Integer, ByVal RS232 As Integer, ByVal Bluetooth As Integer _
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_IsConnected")>
    Public Function IsConnected(
        ByVal SerialNumber As String, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_AreConnected")>
    Public Function AreConnected(
        ByVal PortNumbers() As Integer, ByVal SerialNumbers() As String, ByVal nSerials As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_AreConnectedS")>
    Public Function AreConnectedS(
        ByVal PortNumbers() As Integer, ByVal SerialNumbers As String, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Detect")>
    Public Function Detect(
        ByVal CommPorts() As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_DetectSN")>
    Public Function DetectSN(
        <[In], Out> ByVal SerialNumbers() As String, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_AddDetectionFilter")>
    Public Sub AddDetectionFilter(
        ByVal Filter As String
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ClearDetectionFilters")>
    Public Sub ClearDetectionFilters(
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_IsPortAvailable")>
    Public Function IsPortAvailable(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ListPortsDetected")>
    Public Function ListPortsDetected(
        ByVal ListOfPortsDetected() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_ListPortsDetectedTxt")>
    Public Function ListPortsDetectedTxt(
        ByVal ListOfPortsDetected As StringBuilder, ByVal Delimiter As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_SetResponseTimeout")>
    Public Function SetResponseTimeout(
        ByVal Timeout As UInteger
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_SetResponseTimeoutAuto")>
    Public Function SetResponseTimeoutAuto(
        ByVal CommPort As Integer, ByVal Status As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetBaudrate")>
    Public Function GetBaudrate(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetDeviceType")> _
    Public Function GetDeviceType( _
        ByVal CommPort As Integer, ByVal DeviceType As StringBuilder _
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetError_Description")>
    Public Sub GetError_Description(
        ByVal ErrorDescription As StringBuilder
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetError_DescriptionByPort")>
    Public Sub GetError_DescriptionByPort(
        ByVal CommPort As Integer, ByVal ErrorDescription As StringBuilder
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetError_DescriptionBySN")>
    Public Sub GetError_DescriptionBySN(
        ByVal SerialNumber As String, ByVal ErrorDescription As StringBuilder
        )
    End Sub

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetPortBySN")>
    Public Function GetPortBySN(
        ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetSNByPort")>
    Public Function GetSNByPort(
        ByVal SerialNumber As StringBuilder, ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetPortByID")>
    Public Function GetPortByID(
        ByVal DeviceID As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetOpenedPorts")>
    Public Function GetOpenedPorts(
        ByVal CommPorts() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_GetOpenedPortsS")>
    Public Function GetOpenedPortsS(
        ByVal CommPortsTxt As StringBuilder, ByVal Delimiter as Char
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_OpenProject")>
    Public Function OpenProject(
        ByVal Path As String
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_CloseProject")>
    Public Function CloseProject() As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_SendByID")>
    Public Function SendByID(
        ByVal DeviceID As String, ByVal Command As String, ByVal ResponseText As StringBuilder _
        ) As Integer
    End Function


    ' Binning
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_Binning_GetBinFromVECFile")>
    Public Function Binning_GetBinFromVECFile(
        ByVal Path As String, ByVal x As Single, ByVal y As Single, ByVal ResultBinName As StringBuilder
        ) As Integer
    End Function


    ' UserCal functions
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_ResetIntensity")>
    Public Function UserCal_ResetIntensity(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_GetIntensityGain")>
    Public Function UserCal_GetIntensityGain(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef Gain As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_SetIntensityGain")>
    Public Function UserCal_SetIntensityGain(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Gain As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_AdjustIntensity")>
    Public Function UserCal_AdjustIntensity(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal IntensityRef As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_ResetAbsInt")>
    Public Function UserCal_ResetAbsInt(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_GetAbsIntFactor")>
    Public Function UserCal_GetAbsIntFactor(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef Factor As Double
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_SetAbsIntFactor")>
    Public Function UserCal_SetAbsIntFactor(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Factor As Double, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_AdjustAbsInt")>
    Public Function UserCal_AdjustAbsInt(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal AbsIntRef As Double, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_ResetWavelengthOffset")>
    Public Function UserCal_ResetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_GetWavelengthOffset")>
    Public Function UserCal_GetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef WavelengthOffset As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_SetWavelengthOffset")>
    Public Function UserCal_SetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal WavelengthOffset As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_AdjustWavelengthOffset")>
    Public Function UserCal_AdjustWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal WavelengthRef As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_ResetxyOffsets")>
    Public Function UserCal_ResetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_GetxyOffsets")>
    Public Function UserCal_GetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef xOffset As Single, ByRef yOffset As Single
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_SetxyOffsets")>
    Public Function UserCal_SetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xOffset As Single, ByVal yOffset As Single, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_AdjustxyOffsets")>
    Public Function UserCal_AdjustxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xRef As Single, ByVal yRef As Single, ByVal toFlash As Integer
        ) As Integer
    End Function
	
	
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_ResetRGBAdj")> _
    Public Function UserCal_ResetRGBAdj( _
        ByVal CommPort As Integer, ByVal Fiber As Integer _
        ) As Integer
    End Function
	
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_TakeRGBCurrentValues")> _
    Public Function UserCal_TakeRGBCurrentValues( _
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Color As Char _
        ) As Integer
    End Function
	
    <DllImport(DLL_FEASACOM, EntryPoint:="FeasaCom_UserCal_AdjustRGB")> _
    Public Function UserCal_AdjustRGB( _
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xRefRed As Single, ByVal yRefRed As Single, ByRef AbsIntRefRed As Double, ByVal xRefGreen As Single, ByVal yRefGreen As Single, ByRef AbsIntRefGreen As Double, ByVal xRefBlue As Single, ByVal yRefBlue As Single, ByRef AbsIntRefBlue As Double _
        ) As Integer
    End Function

End Module


Module FeasaTools

    ' Note: feasa_tools.dll file has to be used for 32bit targets
    Private Const DLLTOOLS_PATH As String = "feasa_tools64.dll"

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Int16")>
    Public Function Parse_Int16(
        ByVal StringToParse As String, ByVal Parameter As Byte
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Int32")>
    Public Function Parse_Int32(
        ByVal StringToParse As String, ByVal Parameter As Byte
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Float")>
    Public Function Parse_Float(
        ByVal StringToParse As String, ByVal Parameter As Byte
        ) As Single
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_RGBI")>
    Public Function Parse_RGBI(
        ByVal Response As String, ByRef Red As Byte, ByRef Green As Byte, ByRef Blue As Byte, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_RGBI_All")>
    Public Function Parse_RGBI_All(
        ByVal Response As String, ByVal RedValues() As Byte, ByVal GreenValues() As Byte, ByVal BlueValues() As Byte, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_HSI")>
    Public Function Parse_HSI(
        ByVal Response As String, ByRef Hue As Single, ByRef Saturation As Integer, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_HSI_All")>
    Public Function Parse_HSI_All(
        ByVal Response As String, ByVal HueValues() As Single, ByVal SaturationValues() As Integer, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_xy")>
    Public Function Parse_xy(
        ByVal Response As String, ByRef x As Single, ByRef y As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_xy_All")>
    Public Function Parse_xy_All(
        ByVal Response As String, ByVal xValues() As Single, ByVal yValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_uv")>
    Public Function Parse_uv(
        ByVal Response As String, ByRef u As Single, ByRef v As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_uv_All")>
    Public Function Parse_uv_All(
        ByVal Response As String, ByVal uValues() As Single, ByVal vValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_CCT")>
    Public Function Parse_CCT(
        ByVal Response As String, ByRef CCT As Integer, ByRef delta As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_CCT_All")>
    Public Function Parse_CCT_All(
        ByVal Response As String, ByVal CCTValues() As Integer, ByVal deltaValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Wavelength")>
    Public Function Parse_Wavelength(
        ByVal Response As String, ByRef Wavelength As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Wavelength_All")>
    Public Function Parse_Wavelength_All(
        ByVal Response As String, ByVal WavelengthValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_WI")>
    Public Function Parse_WI(
        ByVal Response As String, ByRef Wavelength As Single, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_WI_All")>
    Public Function Parse_WI_All(
        ByVal Response As String, ByVal WavelengthValues() As Single, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Intensity")>
    Public Function Parse_Intensity(
        ByVal Response As String, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Intensity_All")>
    Public Function Parse_Intensity_All(
        ByVal Response As String, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Spectrum")>
    Public Function Parse_Spectrum(
        ByVal Response As String, ByVal WavelengthValues() As Single, ByVal IntensityValues() As Double
        ) As Integer
    End Function


    Public Sub InitializeArrayOfStrings(ByRef mArray() As String, ByVal StringSize As Integer)
        Dim i As Integer
        For i = 0 To mArray.Length - 1
            mArray(i) = New String(Chr(0), StringSize)
        Next
    End Sub
	
	Public Function FormatDecimal(Number As String) As String

        Dim DecimalFormatDot As Boolean = False

        'Set the decimal format
        Dim auxfloat As Single = 0
        If Single.TryParse("3.21", auxfloat) Then
            If (auxfloat = 3.21F) Then
                DecimalFormatDot = True
            Else
                DecimalFormatDot = False
            End If
        Else
            DecimalFormatDot = False
        End If

        If (DecimalFormatDot) Then
            Return Number.Replace(",", ".")
        Else
            Return Number.Replace(".", ",")
        End If

    End Function
	
End Module