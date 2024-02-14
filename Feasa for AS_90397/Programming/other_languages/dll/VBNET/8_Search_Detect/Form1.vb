'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: Search & Detect
'
'  DESCRIPTION: This example demonstrates how to list
'  all available Feasa devices, and also to locate
'  the port number of a connected Device based on its serial
'  number.
'
'  This example uses a dynamic library to access to the
'  functions related to the LED Analyser. This library is read
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
'***************************************************************

Imports System.Text

Public Class Form1

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub


    Private Sub cmdSearch_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdSearch.Click

        'variables
        Dim buffer As New StringBuilder(1048)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim SerialNumber As String

        Me.Enabled = False 'Disables form
        Cursor.Current = Cursors.WaitCursor 'change cursor to hourglass

        'Check SN
        If Len(txtSN.Text) < 4 Then
            MsgBox("Bad Serial Number", vbOKOnly + vbInformation, "Error")
            Me.Enabled = True 'Enables form
            Cursor.Current = Cursors.Default 'default cursor
            Exit Sub
        End If

        'Get the serial number
        SerialNumber = Strings.Right("0000" & txtSN.Text, 4) 'Force a 4-digits format

        'Clear the Results box
        txtLog.Text = vbNullString

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Log
        txtLog.Text = txtLog.Text & "Searching Led Analyser..." & vbCrLf

        'Find out if the Led Analyser is connected
        portNumber = FeasaCom_IsConnected(SerialNumber, "57600")
        If portNumber = -1 Then
            txtLog.Text = txtLog.Text & "The Led Analyser with the SN:" & SerialNumber & " was not found" & vbCrLf
            Me.Enabled = True 'Enables form
            Cursor.Current = Cursors.Default 'default cursor
            Exit Sub
        Else
            'Log
            txtLog.Text = txtLog.Text & "Led Analyser found on port " & CStr(portNumber) & vbCrLf
        End If

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then

            'Setup special conditions for "long response commands"
            Call FeasaCom_SetResponseTimeout(150) 'timeout to 500 ms

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, "GETSTATUS", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'default cursor
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'default cursor
                Exit Sub
            Else
                'Shows info text
                txtLog.Text = txtLog.Text & buffer.ToString & vbCrLf
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

        Me.Enabled = True 'Enables form
        Cursor.Current = Cursors.Default 'default cursor

    End Sub

    Private Sub btnDetectPorts_Click(sender As Object, e As EventArgs) Handles btnDetectPorts.Click
        Dim Ports(100) As Integer
        Dim nDetected As Integer
        Dim aux As String
        Dim i As Integer

        'Clear the Results box
        txtLog.Clear()

        'Re-enumerate ports (in case anything has been plugged Or unplugged)
        FeasaCom_EnumPorts()

        'Perform detection
        nDetected = FeasaCom_Detect(Ports, "AUTO")
        If nDetected > 0 Then
            'List devices detected
            aux = "Devices detected:" & vbCrLf
            For i = 0 To nDetected - 1
                aux = aux + "...on port COM" & Ports(i).ToString() & vbCrLf
            Next i
            txtLog.Text = String.Concat(txtLog.Text, aux.ToString(), vbCrLf)
        Else
            MessageBox.Show("No devices detected!", "No devices!", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If
    End Sub

    Private Sub btnDetectSerials_Click(sender As Object, e As EventArgs) Handles btnDetectSerials.Click
        Dim lstSerials(50) As String
        Dim nDetected As Integer
        Dim aux As String
        Dim i As Integer

        'Clear the Results box
        txtLog.Clear()

        'Re-enumerate ports (in case anything has been plugged Or unplugged)
        FeasaCom_EnumPorts()

        'Initialize string for responses (necessary to allocate space)
        ' Has to be done just before receiving any response
        InitializeArrayOfStrings(lstSerials, 10)

        'Perform detection
        nDetected = FeasaCom_DetectSN(lstSerials, "AUTO")
        If nDetected > 0 Then
            'List devices detected
            aux = "Devices detected:" & vbCrLf
            For i = 0 To nDetected - 1
                aux = aux + "...SN " & lstSerials(i).ToString() & vbCrLf
            Next i
            txtLog.Text = String.Concat(txtLog.Text, aux.ToString(), vbCrLf)
        Else
            MessageBox.Show("No devices detected!", "No devices!", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Sub InitializeArrayOfStrings(ByRef mArray() As String, ByVal StringSize As Integer)
        'Function extracted from Feasa.vb - Copyright (c) Feasa Enterprises Ltd 2019
        Dim i As Integer
        For i = 0 To mArray.Length - 1
            mArray(i) = New String(Chr(0), StringSize)
        Next
    End Sub

End Class
