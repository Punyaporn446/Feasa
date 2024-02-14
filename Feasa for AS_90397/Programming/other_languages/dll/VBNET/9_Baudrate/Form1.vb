'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: baudrate
'
'  DESCRIPTION: This example demonstrates how to establish
'  a communication with the Feasa LED Analyser picking a
'  known baudrate or detecting it automatically.
'
'  Important Note: it is not possible to communicate to a
'  Led Analyser that does not have axactly the same
'  baudrate used to open the port.
'  Factory default: 57600 baud.
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

        'Add the possible Baudrates
        lstBaudrate.Items.Add("9600")
        lstBaudrate.Items.Add("19200")
        lstBaudrate.Items.Add("38400")
        lstBaudrate.Items.Add("57600")
        lstBaudrate.Items.Add("115200")
        lstBaudrate.Items.Add("230400")
        lstBaudrate.Items.Add("460800")
        lstBaudrate.Items.Add("921600")
        lstBaudrate.SelectedIndex = 3

    End Sub

    Private Sub cmdRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRead.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim aux As String

        Me.Enabled = False 'Disables form
        Cursor.Current = Cursors.WaitCursor 'set cursor to hourglass

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
        If optBaudrateAuto.Checked = True Then
            resp = FeasaCom_Open(portNumber, "AUTO")
        Else
            resp = FeasaCom_Open(portNumber, lstBaudrate.SelectedItem.ToString)
        End If
        If resp = 1 Then

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, "CAPTURE", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'restore cursor
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'restore cursor
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
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'restore cursor
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True 'Enables form
                Cursor.Current = Cursors.Default 'restore cursor
                Exit Sub
            End If

            'Shows the received data in the screen
            txtLog.Text = txtLog.Text & buffer.ToString & vbCrLf

            'Shows baudrate detected
            If (optBaudrateAuto.Checked) Then
                txtLog.Text = txtLog.Text & FeasaCom_GetBaudrate(portNumber).ToString & vbCrLf
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

        Me.Enabled = True 'Enables form
        Cursor.Current = Cursors.Default 'restore cursor

    End Sub

    Private Sub optBaudrateAuto_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles optBaudrateAuto.CheckedChanged
        lstBaudrate.Enabled = False
    End Sub

    Private Sub optBaudrateManual_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles optBaudrateManual.CheckedChanged
        lstBaudrate.Enabled = True
    End Sub
End Class
