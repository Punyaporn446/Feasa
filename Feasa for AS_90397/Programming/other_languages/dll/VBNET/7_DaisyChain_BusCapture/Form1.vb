'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: DaisyChain (Bus capture)
'
'  DESCRIPTION: This example demonstrates how to perform
'  a capture for all Daisy-chained analysers, through the DLL
'  functions and then retrieve the HSI values for the fiber
'  requested.
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
        For i = 1 To 100
            If FeasaCom_IsPortAvailable(i) = 1 Then lstPorts.Items.Add(CStr(i))
        Next i
        If (lstPorts.Items.Count > 0) Then lstPorts.SelectedIndex = 0

        'Fill list with Capture Modes
        lstCaptureMode.Items.Add("AUTO")
        lstCaptureMode.Items.Add("MANUAL")
        lstCaptureMode.SelectedIndex = 0

        'Fill list with Capture Ranges
        lstCaptureRange.Items.Add("LOW")
        lstCaptureRange.Items.Add("MEDIUM")
        lstCaptureRange.Items.Add("HIGH")
        lstCaptureRange.Items.Add("SUPER")
        lstCaptureRange.Items.Add("ULTRA")
        lstCaptureRange.SelectedIndex = 2

    End Sub

    Private Sub cmdRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdRead.Click

        'variables
        Dim buffer As New StringBuilder(100)
        Dim portNumber As Integer
        Dim resp As Integer
        Dim CaptureRange As Integer
        Dim numfib As String
        Dim i As Integer
        Dim SN_main As String

        'Clear the Results box
        txtLog.Text = vbNullString

        'Set the port number
        portNumber = Integer.Parse(lstPorts.Items(lstPorts.SelectedIndex))

        'Check if Analysers has been added to the list
        If lstAnalysers.Items.Count = 0 Then
            MsgBox("Please add Analysers to the list first!", vbOKOnly + vbExclamation, "Error")
            Exit Sub
        End If

        'This command enumerates the existing ports to find out what are the serial
        'ports available on your computer and the devices connected to them.
        'This function needs to be executed any time that a Feasa device is
        'pluged or unpluged, while the application is running
        'Call FeasaCom_EnumPorts()

        'Compose the capture command
        If lstCaptureMode.SelectedIndex = 0 Then
            CaptureRange = 0
        ElseIf lstCaptureMode.SelectedIndex = 1 Then
            CaptureRange = lstCaptureRange.SelectedIndex + 1
        End If

        'Open port
        If FeasaCom_Open(portNumber, "57600") = 1 Then

            'Add Analyser to the bus
            For i = 0 To lstAnalysers.Items.Count - 1
                resp = FeasaCom_DaisyChain_Add(portNumber, lstAnalysers.Items(i))
                If resp = 0 Then
                    MsgBox("Unable to add Analyser to the Bus", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Exit Sub
                End If
            Next

            'Perform Capture
            resp = FeasaCom_DaisyChain_Capture(portNumber, 0, CaptureRange, 0)
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
            End If

            'Free bus owner
            resp = FeasaCom_Send(portNumber, "BUSFREE", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            End If

            'Retrieve Serial number of the Main Analyser
            resp = FeasaCom_Send(portNumber, "GETSERIAL", buffer)
            If resp = -1 Then
                MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            ElseIf resp = 0 Then
                MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                Call FeasaCom_Close(portNumber)
                Exit Sub
            End If
            SN_main = buffer.ToString

            numfib = Strings.Right("00" & txtFiber.Text, 2) 'read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc

            'Get measurements from main Analayser
            resp = FeasaCom_Send(portNumber, "GETHSI" & numfib, buffer)
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
            txtLog.Text = txtLog.Text & "Fib " + numfib + " (" + SN_main + "): " & buffer.ToString & vbCrLf

            'Get measurements from Analysers attached to the bus
            For i = 0 To lstAnalysers.Items.Count - 1
                'Set owner of the bus
                resp = FeasaCom_Send(portNumber, "BUSGET" & lstAnalysers.Items(i), buffer)
                If resp = -1 Then
                    MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Exit Sub
                ElseIf resp = 0 Then
                    MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Exit Sub
                End If

                'Get measurements from main Analayser
                resp = FeasaCom_Send(portNumber, "GETHSI" & numfib, buffer)
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
                txtLog.Text = txtLog.Text & "Fib " + numfib + " (" + lstAnalysers.Items(i) + "): " & buffer.ToString & vbCrLf

                'Free the bus
                resp = FeasaCom_Send(portNumber, "BUSFREE", buffer)
                If resp = -1 Then
                    MsgBox("Error! unable to send the command!", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Exit Sub
                ElseIf resp = 0 Then
                    MsgBox("Timeout detected", vbOKOnly + vbExclamation, "Error")
                    Call FeasaCom_Close(portNumber)
                    Exit Sub
                End If
            Next

            'Close the port
            Call FeasaCom_Close(portNumber)

        Else
            'Error
            MsgBox("Unable to open the port!", vbOKOnly + vbExclamation, "Error")
        End If

    End Sub


    Private Sub cmdAnalysers_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdAnalysers.Click
        Dim sn As String

        sn = InputBox("Please, enter Serial Number of the Analyser to be added to the Bus:", "Add Analyser to Daisy Chain")

        If sn.Length > 0 Then
            lstAnalysers.Items.Add(sn)
        End If
    End Sub
End Class
