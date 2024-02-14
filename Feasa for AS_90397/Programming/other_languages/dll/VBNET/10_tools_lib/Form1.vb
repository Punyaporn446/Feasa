'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: tools_lib
'
'  DESCRIPTION: This example demonstrates how to use the
'  library feasa_tools64.dll or its 32-bit equivalent
'  feasa_tools.dll, to extract or parse the strings returned
'  by the Feasa LED Analyser responses and convert them
'  into usable numerical values.
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
        Dim nPorts As Integer
        Dim PortsList(100) As Integer

        'This command enumerates the existing ports to find out
        'what are the serial ports existing on your computer and
        'the devices connected to them. You need to execute this
        'command everytime you plug or unplug a USB Led Analyser
        Call FeasaCom_EnumPorts()

        'Retrieve the list of existing ports
        nPorts = FeasaCom_ListPortsDetected(PortsList(0))

        'Fill in the list with the existing port numbers
        If nPorts > 0 Then
            For i = 0 To nPorts - 1
                lstPorts.Items.Add(CStr(PortsList(i)))
            Next i
            lstPorts.SelectedIndex = 0
        End If


        'Fill the list with the number of fibers
        lstNumFibs.Items.Add("20")
        lstNumFibs.Items.Add("15")
        lstNumFibs.Items.Add("10")
        lstNumFibs.Items.Add("3")
        lstNumFibs.Items.Add("2")
        lstNumFibs.SelectedIndex = 0 'select 20 fibers as default

        'Setup data grid
        dataGrid.RowCount = 20
        'Initialize grid values
        For i = 0 To 19
            dataGrid.Rows(i).Cells(0).Value = 0
            dataGrid.Rows(i).Cells(1).Value = 0
            dataGrid.Rows(i).Cells(2).Value = 0
            dataGrid.Rows(i).Cells(3).Value = 0
            dataGrid.Rows(i).Cells(4).Value = 0
            dataGrid.Rows(i).Cells(5).Value = 0
        Next i

    End Sub

    Private Sub cmdRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRead.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim fibnum As String
        Dim NumFibers As Integer
        Dim Sensor As Integer
        Dim i As Integer
        Dim Red As Byte, Green As Byte, Blue As Byte
        Dim Intensity As Integer
        Dim Hue As Single
        Dim Saturation As Integer

        Me.Enabled = False  'Disables form

        'Initialize grid values
        For i = 0 To 19
            dataGrid.Rows(i).Cells(0).Value = 0
            dataGrid.Rows(i).Cells(1).Value = 0
            dataGrid.Rows(i).Cells(2).Value = 0
            dataGrid.Rows(i).Cells(3).Value = 0
            dataGrid.Rows(i).Cells(4).Value = 0
            dataGrid.Rows(i).Cells(5).Value = 0
        Next i

        'Get the port to connect to
        portNumber = Convert.ToInt32(lstPorts.Items(lstPorts.SelectedIndex))

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
                fibnum = Strings.Right("00" & CStr(Sensor), 2) 'read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc

                'You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, "GETRGBI" & fibnum, buffer)
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

                'Parse values
                Feasa_Parse_RGBI(buffer.ToString, Red, Green, Blue, Intensity)

                'You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, "GETHSI" & fibnum, buffer)
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

                'Parse values
                Feasa_Parse_HSI(buffer.ToString, Hue, Saturation, Intensity)

                'Shows the received data in the screen
                'Note in the follofing lines that -1 is added because the index should start from 0
                dataGrid.Rows(Sensor - 1).Cells(0).Value = Red 'Red
                dataGrid.Rows(Sensor - 1).Cells(1).Value = Green 'Green
                dataGrid.Rows(Sensor - 1).Cells(2).Value = Blue 'Blue
                dataGrid.Rows(Sensor - 1).Cells(3).Value = Intensity 'Intensity
                dataGrid.Rows(Sensor - 1).Cells(4).Value = Hue 'Hue
                dataGrid.Rows(Sensor - 1).Cells(5).Value = Saturation 'Saturation
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

        Me.Enabled = False

        'Get the port to connect to
        portNumber = Convert.ToInt32(lstPorts.Items(lstPorts.SelectedIndex))

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

    Private Sub DataGridView1_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dataGrid.CellContentClick

    End Sub
End Class
