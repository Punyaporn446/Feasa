Option Strict On
Option Explicit On

Imports System.Runtime.InteropServices
Imports System.Text

Module DLLconf

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_EnumPorts() As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_Open( _
        ByVal CommPort As Integer, ByVal Baudrate As String _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_Close( _
        ByVal CommPort As Integer _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_Send( _
        ByVal CommPort As Integer, ByVal Command As String, ByVal ResponseText As StringBuilder _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_ListPortsDetected( _
        ByRef ListOfPortsDetected As Integer _
        ) As Integer
    End Function

    <DllImport("feasa_tools64.dll")> _
    Public Function Feasa_Parse_RGBI( _
    ByVal AnalyserResponse As String, ByRef Red As Byte, ByRef Green As Byte, ByRef Blue As Byte, ByRef Intensity As Integer _
    ) As Single
    End Function

    <DllImport("feasa_tools64.dll")> _
    Public Function Feasa_Parse_HSI( _
    ByVal AnalyserResponse As String, ByRef Hue As Single, ByRef Saturation As Integer, ByRef Intensity As Integer _
    ) As Single
    End Function


End Module
