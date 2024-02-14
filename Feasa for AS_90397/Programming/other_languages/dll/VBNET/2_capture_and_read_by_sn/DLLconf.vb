Option Strict On
Option Explicit On

Imports System.Runtime.InteropServices
Imports System.Text

Module DLLconf

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_EnumPorts() As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_OpenSN( _
        ByVal SerialNumber As String, ByVal Baudrate As String _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_CloseSN( _
        ByVal SerialNumber As String _
        ) As Integer
    End Function

    <DllImport("feasacom64.dll")> _
    Public Function FeasaCom_SendSN( _
        ByVal SerialNumber As String, ByVal Command As String, ByVal ResponseText As StringBuilder _
        ) As Integer
    End Function

End Module
