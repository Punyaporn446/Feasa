'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: Fast Test (Multi-threaded)
'
'  DESCRIPTION: This example demonstrates how to use the multi-
'  threaded functions provided in the DLL to set up a fast and
'  efficient communication schema for your application.
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
'**************************************************************

Imports System.Text

Public Class Form1

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        'variables
        Dim i As Integer

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Add the existing port numbers
        For i = 1 To 100
            If FeasaCom.IsPortAvailable(i) Then lstPorts.Items.Add(CStr(i))
        Next i
        lstPorts.SelectedIndex = 0

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

    Private Sub cmdCapture_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdCapture.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim resp As Integer
        Dim PortCount As Integer
        Dim dtIni As DateTime
        Dim i As Integer

        Me.Enabled = False
        dtIni = DateTime.Now

        If lstPortsToTest.Items.Count = 0 Then
            MsgBox("Please, add at least one port to the list of devices to be tested.", vbOKOnly + vbExclamation, "No ports added")
            Exit Sub
        End If

        'Get the number of ports added to the list
        PortCount = lstPortsToTest.Items.Count

        'Initialize variables
        Dim ReturnValues(PortCount) As Integer
        Dim Ports(PortCount) As Integer
        Dim Responses(PortCount) As String
        Dim SNs(PortCount) As String

        For i = 0 To PortCount - 1
            Ports(i) = Convert.ToInt32(lstPortsToTest.Items(i))
        Next

        'Open port
        If FeasaCom.Open_Multi(ReturnValues, Ports, PortCount, "57600") = 1 Then
            'No error

            'Retrieve Serial numbers connected
            For i = 0 To PortCount - 1
                resp = FeasaCom.GetSNByPort(buffer, Ports(i))
                If resp = 1 Then SNs(i) = buffer.ToString() Else SNs(i) = ""
            Next

            'Initialize string for responses (necessary to allocate space)
            ' Has to be done just before receiving any response
            FeasaTools.InitializeArrayOfStrings(Responses, 100)

            'Send command to All Analysers connected
            resp = FeasaCom.SendToAll(ReturnValues, "CAPTURE", Responses)
            If resp <> 1 Then
                For i = 0 To PortCount - 1
                    If ReturnValues(i) = -1 Then
                        MsgBox("Unable to send the command to " & SNs(i) & "!", vbOKOnly + vbExclamation, "Error")
                        FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
                        Me.Enabled = True
                        Exit Sub
                    ElseIf ReturnValues(i) = 0 Then
                        MsgBox("Timeout or Syntax error detected in " & SNs(i) & "!", vbOKOnly + vbExclamation, "Error")
                        FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
                        Me.Enabled = True
                        Exit Sub
                    End If
                Next i
            End If

            'Initialize string for responses (necessary to allocate space)
            ' Has to be done just before receiving any response
            FeasaTools.InitializeArrayOfStrings(Responses, 500)

            'Send command to All Analysers connected
            resp = FeasaCom.SendToAll(ReturnValues, "GETHSIALL", Responses)
            If resp <> 1 Then
                For i = 0 To PortCount - 1
                    If ReturnValues(i) = -1 Then
                        MsgBox("Unable to send the command to " & SNs(i) & "!", vbOKOnly + vbExclamation, "Error")
                        FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
                        Me.Enabled = True
                        Exit Sub
                    ElseIf ReturnValues(i) = 0 Then
                        MsgBox("Timeout or Syntax error detected in " & SNs(i) & "!", vbOKOnly + vbExclamation, "Error")
                        FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)
                        Me.Enabled = True
                        Exit Sub
                    End If
                Next i
            End If

            'Clear grid
            dataGrid.Rows.Clear()

            'Extract response lines and parse responses
            For i = 0 To PortCount - 1
                'Declare numerical arrays for responses with 20 elements (maximum number of channels)
                Dim HueValues(20) As Single
                Dim SaturationValues(20) As Integer
                Dim IntensityValues(20) As Integer
                Dim nLines As Integer
                nLines = FeasaTools.Parse_HSI_All(Responses(i), HueValues, SaturationValues, IntensityValues)
                If (nLines > 0) Then
                    Dim f As Integer
                    For f = 0 To nLines - 1
                        dataGrid.Rows.Add(SNs(i), f + 1, HueValues(f), SaturationValues(f), IntensityValues(f))
                    Next
                End If
            Next

            'Close the port
            FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If


        'Calculates execution time
        Dim ts As TimeSpan = DateTime.Now - dtIni
        lblExecutionTime.Text = "Execution time: " & ts.TotalMilliseconds & " ms"

        Me.Enabled = True

    End Sub

    Private Sub btnAdd_Click(sender As Object, e As EventArgs) Handles btnAdd.Click
        If lstPorts.SelectedIndex >= 0 Then
            'Adds selected port to the list of port to be tested
            lstPortsToTest.Items.Add(lstPorts.Items(lstPorts.SelectedIndex))
            'Removes port from list of ports
            lstPorts.Items.RemoveAt(lstPorts.SelectedIndex)
        End If
    End Sub
End Class
