'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: DaisyChain
'
'  DESCRIPTION: This example demonstrates how to establish
'  a communication with a daisy chained Feasa LED Analyser
'  using commands GETBUS and FREEBUS.
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
        Dim resp As Integer
        Dim aux As String

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

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, "CAPTURE", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            End If

            'Shows the received data in the screen
            txtLog.Text = buffer.ToString & vbCrLf

            'Send command to the Led Analyser
            aux = Strings.Right("00" & txtFiber.Text, 2) 'read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
            'You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
            resp = FeasaCom_Send(portNumber, "GETRGBI" & aux, buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            End If

            'Shows the received data in the screen
            txtLog.Text = txtLog.Text & buffer.ToString & vbCrLf

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

    End Sub

    Private Sub cmdGet_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdGet.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim SN As String

        'Check SN
        If Len(txtSN.Text) < 4 Then
            MsgBox("Bad Serial Number", vbOKOnly + vbInformation, "Error")
            Exit Sub
        End If

        'Get the serial number
        SN = Strings.Right("0000" & txtSN.Text, 4) 'Force a 4-digits format

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

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, "BUSGET" & SN, buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            Else
                'Shows info text
                txtLog.Text = buffer.ToString & vbCrLf & "The Bus belongs to the Led Analyser " & SN
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

    End Sub

    Private Sub cmdFree_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdFree.Click
        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer

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

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, "BUSFREE", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            Else
                'Shows info text
                txtLog.Text = buffer.ToString & vbCrLf & "The bus is Free!"
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

    End Sub
End Class
