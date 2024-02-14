'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  Display Tester examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: getserial
'
'  DESCRIPTION: This example demonstrates how to establish
'  a communication with the Feasa Display Tester. It also shows
'  a method to load the DLL library and to call the functions
'  provided.
'
'  This example uses a dynamic library to access to the
'  functions related to the Display Tester. This library is read
'  in runtime so you have to be sure that the file feasacom64.dll
'  exists in the same location of the EXE or in windows/system32
'  folder, however some compillers allow to reference the DLL
'  library from alternative locations using absolute or relative
'  paths.
'
'  Note: there are 32 and 64-bit versions of the DLL, so one or
'  the other has to be used depending on the compiler/IDE platform
'  or binary target platform.
'
'**************************************************************

Imports System.Text

Public Class Form1

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        'variables
        Dim i As Integer

        'List available ports
        For i = 0 To 100
            lstPorts.Items.Add(CStr(i))
        Next i
        lstPorts.SelectedIndex = 1

    End Sub

    Private Sub cmdRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRead.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer

        'Clear the Results box
        txtLog.Text = vbNullString

        'Set the port number
        portNumber = lstPorts.SelectedIndex

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then
            'No error

            'Send command to the Display Tester
            'You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
            Call FeasaCom_Send(portNumber, "GETSERIAL", buffer)

            'Shows the received data in the screen
            txtLog.Text = "Serial:" & buffer.ToString

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

    End Sub
End Class
