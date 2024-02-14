Option Strict On
Option Explicit On

Imports System.Runtime.InteropServices
Imports System.Text

Module FeasaCom

    ' Note: feasacom.dll file has to be used for 32bit targets
    Private Const DLL_PATH As String = "feasacom64.dll"

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

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SendToAll")>
    Public Function SendToAll(
        ByVal ReturnValues() As Integer, ByVal Command As String, <[In], Out> ByVal Responses() As String
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

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_OpenSN_Multi")>
    Public Function OpenSN_Multi(
        ByVal ReturnValues() As Integer, ByVal SerialNumbers() As String, ByVal nPorts As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CloseSN_Multi")>
    Public Function CloseSN_Multi(
        ByVal ReturnValues() As Integer, ByVal SerialNumbers() As String, ByVal nPorts As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_CloseAll")>
    Public Function CloseAll(
        ) As Integer
    End Function


    ' Daisy-chain helper functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_DaisyChain_Add")>
    Public Function DaisyChain_Add(
        ByVal CommPort As Integer, ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_DaisyChain_Del")>
    Public Function DaisyChain_Del(
        ByVal CommPort As Integer, ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_DaisyChain_Clear")>
    Public Function DaisyChain_Clear(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_DaisyChain_Capture")>
    Public Function DaisyChain_Capture(
        ByVal CommPort As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal CapturePWMFrames As Integer
        ) As Integer
    End Function


    ' Comm handling functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_EnumPorts")>
    Public Function EnumPorts() As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_IsConnected")>
    Public Function IsConnected(
        ByVal SerialNumber As String, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_AreConnected")>
    Public Function AreConnected(
        ByVal SerialNumbers() As String, ByVal nSerials As Integer, ByVal Baudrate As String, ByVal PortNumbers() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_AreConnectedS")>
    Public Function AreConnectedS(
        ByVal SerialNumbers As String, ByVal Baudrate As String, ByVal PortNumbers() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_SetResponseTimeout")>
    Public Function SetResponseTimeout(
        ByVal Timeout As UInteger
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_IsPortAvailable")>
    Public Function IsPortAvailable(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_ListPortsDetected")>
    Public Function ListPortsDetected(
        ByVal ListOfPortsDetected() As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_ListPortsDetectedTxt")>
    Public Function ListPortsDetectedTxt(
        ByVal ListOfPortsDetected As StringBuilder, ByVal Delimiter As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetBaudrate")>
    Public Function GetBaudrate(
        ByVal CommPort As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetError_Description")>
    Public Function GetError_Description(
        ByVal ErrorDescription As StringBuilder
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetPortBySN")>
    Public Function GetPortBySN(
        ByVal SerialNumber As String
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_GetSNByPort")>
    Public Function GetSNByPort(
        ByVal SerialNumber As StringBuilder, ByVal CommPort As Integer
        ) As Integer
    End Function


    ' Binning
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_Binning_GetBinFromVECFile")>
    Public Function Binning_GetBinFromVECFile(
        ByVal Path As String, ByVal x As Single, ByVal y As Single, ByVal ResultBinName As StringBuilder
        ) As Integer
    End Function


    ' UserCal functions
    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_ResetIntensity")>
    Public Function UserCal_ResetIntensity(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_GetIntensityGain")>
    Public Function UserCal_GetIntensityGain(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef Gain As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_SetIntensityGain")>
    Public Function UserCal_SetIntensityGain(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Gain As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_AdjustIntensity")>
    Public Function UserCal_AdjustIntensity(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal IntensityRef As Integer, ByVal isPWM As Integer, ByVal CaptureRange As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_ResetAbsInt")>
    Public Function UserCal_ResetAbsInt(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_GetAbsIntFactor")>
    Public Function UserCal_GetAbsIntFactor(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef Factor As Double
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_SetAbsIntFactor")>
    Public Function UserCal_SetAbsIntFactor(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Factor As Double, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_AdjustAbsInt")>
    Public Function UserCal_AdjustAbsInt(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal AbsIntRef As Double, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_ResetWavelengthOffset")>
    Public Function UserCal_ResetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_GetWavelengthOffset")>
    Public Function UserCal_GetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef WavelengthOffset As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_SetWavelengthOffset")>
    Public Function UserCal_SetWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal WavelengthOffset As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_AdjustWavelengthOffset")>
    Public Function UserCal_AdjustWavelengthOffset(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal WavelengthRef As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_ResetxyOffsets")>
    Public Function UserCal_ResetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_GetxyOffsets")>
    Public Function UserCal_GetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByRef xOffset As Single, ByRef yOffset As Single
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_SetxyOffsets")>
    Public Function UserCal_SetxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xOffset As Single, ByVal yOffset As Single, ByVal toFlash As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_AdjustxyOffsets")>
    Public Function UserCal_AdjustxyOffsets(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xRef As Single, ByVal yRef As Single, ByVal toFlash As Integer
        ) As Integer
    End Function


    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_ResetRGBAdj")>
    Public Function UserCal_ResetRGBAdj(
        ByVal CommPort As Integer, ByVal Fiber As Integer
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_TakeRGBCurrentValues")>
    Public Function UserCal_TakeRGBCurrentValues(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal Color As Char
        ) As Integer
    End Function

    <DllImport(DLL_PATH, EntryPoint:="FeasaCom_UserCal_AdjustRGB")>
    Public Function UserCal_AdjustRGB(
        ByVal CommPort As Integer, ByVal Fiber As Integer, ByVal xRefRed As Single, ByVal yRefRed As Single, ByVal AbsIntRefRed As Double, ByVal xRefGreen As Single, ByVal yRefGreen As Single, ByVal AbsIntRefGreen As Double, ByVal xRefBlue As Single, ByVal yRefBlue As Single, ByVal AbsIntRefBlue As Double
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
        ByVal AnalyserResponse As String, ByRef Red As Byte, ByRef Green As Byte, ByRef Blue As Byte, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_RGBI_All")>
    Public Function Parse_RGBI_All(
        ByVal AnalyserResponse As String, ByVal RedValues() As Byte, ByVal GreenValues() As Byte, ByVal BlueValues() As Byte, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_HSI")>
    Public Function Parse_HSI(
        ByVal AnalyserResponse As String, ByRef Hue As Single, ByRef Saturation As Integer, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_HSI_All")>
    Public Function Parse_HSI_All(
        ByVal AnalyserResponse As String, ByVal HueValues() As Single, ByVal SaturationValues() As Integer, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_xy")>
    Public Function Parse_xy(
        ByVal AnalyserResponse As String, ByRef x As Single, ByRef y As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_xy_All")>
    Public Function Parse_xy_All(
        ByVal AnalyserResponse As String, ByVal xValues() As Single, ByVal yValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_uv")>
    Public Function Parse_uv(
        ByVal AnalyserResponse As String, ByRef u As Single, ByRef v As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_uv_All")>
    Public Function Parse_uv_All(
        ByVal AnalyserResponse As String, ByVal uValues() As Single, ByVal vValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_CCT")>
    Public Function Parse_CCT(
        ByVal AnalyserResponse As String, ByRef CCT As Integer, ByRef delta As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_CCT_All")>
    Public Function Parse_CCT_All(
        ByVal AnalyserResponse As String, ByVal CCTValues() As Integer, ByVal deltaValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Wavelength")>
    Public Function Parse_Wavelength(
        ByVal AnalyserResponse As String, ByRef Wavelength As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Wavelength_All")>
    Public Function Parse_Wavelength_All(
        ByVal AnalyserResponse As String, ByVal WavelengthValues() As Single
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_WI")>
    Public Function Parse_WI(
        ByVal AnalyserResponse As String, ByRef Wavelength As Single, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_WI_All")>
    Public Function Parse_WI_All(
        ByVal AnalyserResponse As String, ByVal WavelengthValues() As Single, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Intensity")>
    Public Function Parse_Intensity(
        ByVal AnalyserResponse As String, ByRef Intensity As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Intensity_All")>
    Public Function Parse_Intensity_All(
        ByVal AnalyserResponse As String, ByVal IntensityValues() As Integer
        ) As Integer
    End Function

    <DllImport(DLLTOOLS_PATH, EntryPoint:="Feasa_Parse_Spectrum")>
    Public Function Parse_Spectrum(
        ByVal AnalyserResponse As String, ByVal WavelengthValues() As Single, ByVal IntensityValues() As Double
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