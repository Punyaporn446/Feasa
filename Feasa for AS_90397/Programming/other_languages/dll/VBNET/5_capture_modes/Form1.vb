'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: capture_modes
'
'  DESCRIPTION: This example demonstrates the different methods
'  available for performing measurements (captures) on the
'  LED Analyser. Then, the responses received are parsed and
'  the numerical values are exatracted and printed to a
'  grid-style output.
'  This example uses a helper function to format the received
'  decimal string to the default Local Decimal character.
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

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged
        'Call FeasaCom_EnumPorts()

        'Add the existing port numbers
        For i = 1 To 100
            If FeasaCom_IsPortAvailable(i) Then lstPorts.Items.Add(CStr(i))
        Next i
        If (lstPorts.Items.Count > 0) Then lstPorts.SelectedIndex = 0

        'Fill the list with the number of fibers
        lstNumFibs.Items.Add("20")
        lstNumFibs.Items.Add("15")
        lstNumFibs.Items.Add("10")
        lstNumFibs.Items.Add("3")
        lstNumFibs.Items.Add("2")
        lstNumFibs.SelectedIndex = 0 'select 20 fibers as default

        'Fill list with Capture Modes
        lstCaptureMode.Items.Add("AUTO")
        lstCaptureMode.Items.Add("MANUAL")
        lstCaptureMode.Items.Add("PWM: AUTO-RANGE & AUTO-FRAMING")
        lstCaptureMode.Items.Add("PWM: MANUAL-RANGE & AUTO-FRAMING")
        lstCaptureMode.Items.Add("PWM: AUTO-RANGE & MANUAL-FRAMING")
        lstCaptureMode.Items.Add("PWM: MANUAL-RANGE & MANUAL-FRAMING")
        lstCaptureMode.SelectedIndex = 0

        'Fill list with Capture Ranges
        lstCaptureRange.Items.Add("LOW")
        lstCaptureRange.Items.Add("MEDIUM")
        lstCaptureRange.Items.Add("HIGH")
        lstCaptureRange.Items.Add("SUPER")
        lstCaptureRange.Items.Add("ULTRA")
        lstCaptureRange.SelectedIndex = 2

        'Fill list with Capture Frames
        For i = 1 To 15
            lstCaptureFrames.Items.Add(i.ToString())
        Next i
        lstCaptureFrames.SelectedIndex = 4

        'Setup data grid
        dataGrid.RowCount = 20
        'Initialize grid values
        For i = 0 To 19
            dataGrid.Rows(i).Cells(0).Value = 0
            dataGrid.Rows(i).Cells(1).Value = 0
            dataGrid.Rows(i).Cells(2).Value = 0
            dataGrid.Rows(i).Cells(3).Value = 0
        Next i

    End Sub

    Private Sub cmdRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRead.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim aux As String
        Dim auxlist() As String
        Dim NumFibers As Integer
        Dim Sensor As Integer
        Dim i As Integer

        Me.Enabled = False  'Disables form

        'Initialize grid values
        For i = 0 To 19
            dataGrid.Rows(i).Cells(0).Value = 0
            dataGrid.Rows(i).Cells(1).Value = 0
            dataGrid.Rows(i).Cells(2).Value = 0
            dataGrid.Rows(i).Cells(3).Value = 0
        Next i

        'Get the port to connect to (+1 added because index starts from 0)
        portNumber = CInt(lstPorts.Items(lstPorts.SelectedIndex))

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then

            'Get the number of fibers
            NumFibers = Val(lstNumFibs.Text)

            'Loop: executes x readings from 1 to the number of fibers
            For Sensor = 1 To NumFibers

                'Send command to the Led Analyser
                aux = Strings.Right("00" & CStr(Sensor), 2) 'read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                'You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, "GETRGBI" & aux, buffer)
                If resp = -1 Then
                    MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Me.Enabled = True
                    Exit Sub
                ElseIf resp = 0 Then
                    MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Me.Enabled = True
                    Exit Sub
                End If

                'Shows the received data in the screen
                '(Split the response using the char Space as separator and returns the result into a string)
                auxlist = Split(buffer.ToString, " ")
                'Note in the follofing lines that -1 is added because the index should start from 0
                dataGrid.Rows(Sensor - 1).Cells(0).Value = CInt(auxlist(0)) 'Red
                dataGrid.Rows(Sensor - 1).Cells(1).Value = CInt(auxlist(1)) 'Green
                dataGrid.Rows(Sensor - 1).Cells(2).Value = CInt(auxlist(2)) 'Blue
                dataGrid.Rows(Sensor - 1).Cells(3).Value = CInt(auxlist(3)) 'Intensity
            Next Sensor

            'Close the port
            Call FeasaCom_Close(portNumber)
        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

        Me.Enabled = True

    End Sub

    Private Sub cmdCapture_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCapture.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim CaptureComnmand As String

        Me.Enabled = False

        'Get the port to connect to (+1 added because index starts from 0)
        portNumber = CInt(lstPorts.Items(lstPorts.SelectedIndex))

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Increase the maximum timeout in order to avoid timeout event caused by long captures (manual, PWM...)
        Call FeasaCom_SetResponseTimeout(8000) '8000 milliseconds

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then

            'Compose the capture command
            If lstCaptureMode.SelectedIndex = 0 Then
                CaptureComnmand = "CAPTURE"
            ElseIf lstCaptureMode.SelectedIndex = 1 Then
                CaptureComnmand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString()
            ElseIf lstCaptureMode.SelectedIndex = 2 Then
                CaptureComnmand = "CAPTUREPWM"
            ElseIf lstCaptureMode.SelectedIndex = 3 Then
                CaptureComnmand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString() + "PWM"
            ElseIf lstCaptureMode.SelectedIndex = 4 Then
                CaptureComnmand = "CAPTUREPWM" + (lstCaptureFrames.SelectedIndex + 1).ToString("00")
            ElseIf lstCaptureMode.SelectedIndex = 5 Then
                CaptureComnmand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString() + "PWM" + (lstCaptureFrames.SelectedIndex + 1).ToString("00")
            Else
                CaptureComnmand = "CAPTURE"
            End If

            'Send command to the Led Analyser
            resp = FeasaCom_Send(portNumber, CaptureComnmand, buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True
                Exit Sub
            Else
                'Shows info message
                MsgBox("Capture successful!")
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

        Me.Enabled = True

    End Sub

    Private Sub cmdCaptureEasy_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCaptureEasy.Click

        'variables
        Dim portNumber As Integer
        Dim resp As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Dim CapturePWMFrames As Integer

        Me.Enabled = False

        'Get the port to connect to (+1 added because index starts from 0)
        portNumber = CInt(lstPorts.Items(lstPorts.SelectedIndex))

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then

            'Compose the capture command
            If lstCaptureMode.SelectedIndex = 0 Then
                isPWM = 0
                CaptureRange = 0
            ElseIf lstCaptureMode.SelectedIndex = 1 Then
                isPWM = 0
                CaptureRange = lstCaptureRange.SelectedIndex + 1
            ElseIf lstCaptureMode.SelectedIndex = 2 Then
                isPWM = 1
                CaptureRange = 0
                CapturePWMFrames = 0
            ElseIf lstCaptureMode.SelectedIndex = 3 Then
                isPWM = 1
                CaptureRange = lstCaptureRange.SelectedIndex + 1
                CapturePWMFrames = 0
            ElseIf lstCaptureMode.SelectedIndex = 4 Then
                CaptureRange = 0
                CapturePWMFrames = lstCaptureFrames.SelectedIndex + 1
                isPWM = 1
            ElseIf lstCaptureMode.SelectedIndex = 5 Then
                isPWM = 1
                CaptureRange = lstCaptureRange.SelectedIndex + 1
                CapturePWMFrames = lstCaptureFrames.SelectedIndex + 1
            Else
                isPWM = 0
                CaptureRange = 0
            End If

            'Send command to the Led Analyser
            resp = FeasaCom_Capture(portNumber, isPWM, CaptureRange, CapturePWMFrames)
            If resp = -1 Then
                MsgBox("Error! Unable to capture!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Me.Enabled = True
                Exit Sub
            Else
                'Shows info message
                MsgBox("Capture successful!")
            End If

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

        Me.Enabled = True

    End Sub
End Class
