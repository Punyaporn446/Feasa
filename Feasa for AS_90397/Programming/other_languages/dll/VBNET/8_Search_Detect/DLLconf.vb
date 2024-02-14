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
    Public Function FeasaCom_IsConnected( _
        ByVal SerialNumber As String, ByVal Baudrate As String _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_SetResponseTimeout( _
        ByVal Timeout As UInteger _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")>
    Public Function FeasaCom_Detect(
        ByVal CommPorts() As Integer, ByVal Baudrate As String
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")>
    Public Function FeasaCom_DetectSN(
        <[In], Out> ByVal SerialNumbers() As String, ByVal Baudrate As String
        ) As Integer
    End Function

End Module
