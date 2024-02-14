'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: UserCal
'
'  DESCRIPTION: This example demonstrates how to use the
'  UserCal library embedded in the Feasa DLL in order to ease
'  the integration of the calibration process in any user
'  custom software.
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
        'pluged or unpluged, while the application is running
        'Call FeasaCom.EnumPorts()

        'Add the existing port numbers
        For i = 1 To 100
            If FeasaCom.IsPortAvailable(i) Then lstPorts.Items.Add(CStr(i))
        Next i
        lstPorts.SelectedIndex = 0

        'Select the first port of the list
        lstPorts.SelectedIndex = 0

        'Select Capture type
        lstCapture.SelectedIndex = 0

        'Select target
        optRAM.Checked = True

    End Sub

    Private Sub btnBalanceInt_Click(sender As Object, e As EventArgs) Handles btnBalanceInt.Click
        Dim DevicePort As Integer
        Dim resp As Integer
        Dim Buffer As New StringBuilder(1024)
        Dim i As Integer
        Dim toFlash As Integer
        Dim NFIBERS As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Const PWMframes = 5

        'Initialize variables
        If optFlash.Checked Then toFlash = 1 Else toFlash = 0
        NFIBERS = Convert.ToInt32(numFibers.Value)
        If chkPWM.Checked Then isPWM = 1 Else isPWM = 0
        CaptureRange = lstCapture.SelectedIndex

        If lstPorts.SelectedIndex = -1 Then Exit Sub

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), DevicePort) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'This command enumerates the existing ports to find out
        'what are the serial ports existing on your computer And
        'the Devices connected to them. You need to execute this
        'Command everytime you plug Or unplug a Feasa Device,
        'While the application Is running
        'FeasaCom.EnumPorts()

        'Open port
        If FeasaCom.Open(DevicePort, "57600") = 1 Then

            'No error

            '-------------------------------------------------
            ' RELATIVE INTENSITY ADJUSTMENT
            '-------------------------------------------------

            'Reset intensities
            For i = 1 To NFIBERS
                FeasaCom.UserCal_ResetIntensity(DevicePort, i, toFlash)
            Next i

            'Capture
            FeasaCom.Capture(DevicePort, isPWM, CaptureRange, PWMframes)

            'Read back intensities / Calculates average intensity
            Dim AvgInt As Integer = 0
            For i = 1 To NFIBERS
                resp = FeasaCom.Send(DevicePort, "GETINTENSITY" + i.ToString("00"), Buffer)
                If resp = -1 Then
                    MessageBox.Show("Error: unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(DevicePort)
                    Exit Sub
                ElseIf resp = 0 Then
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(DevicePort)
                    Exit Sub
                End If
                Dim auxint As Integer = 0
                Integer.TryParse(Buffer.ToString(), auxint)
                AvgInt += auxint
            Next
            AvgInt = AvgInt / NFIBERS
            txtLog.Text = String.Concat(txtLog.Text, "AvgInt=" + AvgInt.ToString(), vbCrLf)

            'Adjustment
            For i = 1 To NFIBERS
                resp = FeasaCom.UserCal_AdjustIntensity(DevicePort, i, AvgInt, isPWM, CaptureRange, toFlash)
                If resp <> 1 Then
                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(DevicePort)
                    Exit Sub
                End If
            Next

            'Check results
            FeasaCom.Capture(DevicePort, isPWM, CaptureRange, PWMframes) 'Capture
            resp = FeasaCom.Send(DevicePort, "GETINTENSITYALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Exit Sub
            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Exit Sub
            End If
            txtLog.Text = String.Concat(txtLog.Text, "Results:", vbCrLf, Buffer.ToString(), vbCrLf)

            'Close the port
            FeasaCom.Close(DevicePort)

        Else

            'Error: unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If
    End Sub

    Private Sub btnAdjustAbsInt_Click(sender As Object, e As EventArgs) Handles btnAdjustAbsInt.Click

        Dim portNumber As Integer
        Dim resp As Integer
        Dim Buffer As New StringBuilder(1024)
        Dim i As Integer
        Dim toFlash As Integer
        Dim NFIBERS As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Const PWMframes = 5

        'Initialize variables
        If optFlash.Checked Then toFlash = 1 Else toFlash = 0
        NFIBERS = Convert.ToInt32(numFibers.Value)
        If chkPWM.Checked Then isPWM = 1 Else isPWM = 0
        CaptureRange = lstCapture.SelectedIndex

        If lstPorts.SelectedIndex = -1 Then Exit Sub

        If lstCapture.SelectedIndex = -1 Then Exit Sub

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), portNumber) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'Open port
        If FeasaCom.Open(portNumber, "57600") = 1 Then

            'No error

            '-------------------------------------------------
            ' ABSOLUTE INTENSITY ADJUSTMENT
            '-------------------------------------------------

            'Reset factors
            For i = 1 To NFIBERS
                FeasaCom.UserCal_ResetAbsInt(portNumber, i, toFlash)
            Next

            'Capture
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes)

            'Read reference
            Dim AbsIntRef As Double = 0.02355
            Double.TryParse(FeasaTools.FormatDecimal(txtRefAbsInt.Text), AbsIntRef)
            txtLog.Text = String.Concat(txtLog.Text, "AbsIntRef=" + AbsIntRef.ToString(), vbCrLf)

            'Calibration
            For i = 1 To NFIBERS
                resp = FeasaCom.UserCal_AdjustAbsInt(portNumber, i, AbsIntRef, toFlash)
                If (resp <> 1) Then
                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(portNumber)
                    Exit Sub
                End If
            Next

            'Check results
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes) 'Capture
            resp = FeasaCom.Send(portNumber, "GETABSINTALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub

            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub
            End If

            txtLog.Text = String.Concat(txtLog.Text, Buffer.ToString(), vbCrLf)

            'Close the port
            FeasaCom.Close(portNumber)

        Else

            'Error unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub btnAdjustWavelength_Click(sender As Object, e As EventArgs) Handles btnAdjustWavelength.Click

        Dim portNumber As Integer
        Dim resp As Integer
        Dim Buffer As New StringBuilder(1024)
        Dim i As Integer
        Dim toFlash As Integer
        Dim NFIBERS As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Const PWMframes = 5

        'Initialize variables
        If optFlash.Checked Then toFlash = 1 Else toFlash = 0
        NFIBERS = Convert.ToInt32(numFibers.Value)
        If chkPWM.Checked Then isPWM = 1 Else isPWM = 0
        CaptureRange = lstCapture.SelectedIndex

        If lstPorts.SelectedIndex = -1 Then Exit Sub

        If lstCapture.SelectedIndex = -1 Then Exit Sub

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), portNumber) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'Open port
        If FeasaCom.Open(portNumber, "57600") = 1 Then
            'No error

            '-------------------------------------------------
            ' WAVELENGTH OFFSETS ADJUSTMENT
            '-------------------------------------------------

            'Reset offsets
            For i = 1 To NFIBERS
                FeasaCom.UserCal_ResetWavelengthOffset(portNumber, i, toFlash)
            Next

            'Capture
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes)

            Dim Wref As Integer = 0
            Integer.TryParse(txtRefWavelength.Text, Wref)
            txtLog.Text = String.Concat(txtLog.Text, "Wref=" + Wref.ToString(), vbCrLf)

            'Calibration
            For i = 1 To NFIBERS
                resp = FeasaCom.UserCal_AdjustWavelengthOffset(portNumber, i, Wref, toFlash)
                If (resp <> 1) Then

                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(portNumber)
                    Exit Sub
                End If
            Next

            'Check results
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes) 'Capture
            resp = FeasaCom.Send(portNumber, "GETwavelengthALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub

            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub
            End If

            txtLog.Text = String.Concat(txtLog.Text, Buffer.ToString(), vbCrLf)

            'Close the port
            FeasaCom.Close(portNumber)

        Else

            'Error unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub btnAdjustxy_Click(sender As Object, e As EventArgs) Handles btnAdjustxy.Click

        Dim portNumber As Integer
        Dim resp As Integer
        Dim Buffer As New StringBuilder(1024)
        Dim i As Integer
        Dim toFlash As Integer
        Dim NFIBERS As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Const PWMframes = 5

        'Initialize variables
        If optFlash.Checked Then toFlash = 1 Else toFlash = 0
        NFIBERS = Convert.ToInt32(numFibers.Value)
        If chkPWM.Checked Then isPWM = 1 Else isPWM = 0
        CaptureRange = lstCapture.SelectedIndex

        If lstPorts.SelectedIndex = -1 Then Exit Sub

        If lstCapture.SelectedIndex = -1 Then Exit Sub

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), portNumber) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'Open port
        If FeasaCom.Open(portNumber, "57600") = 1 Then
            'No error

            '-------------------------------------------------
            ' xy OFFSETS ADJUSTMENT
            '-------------------------------------------------

            'Reset offsets
            For i = 1 To NFIBERS
                FeasaCom.UserCal_ResetxyOffsets(portNumber, i, toFlash)
            Next

            'Capture
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes)

            'Read back results
            Dim xRef As Single = 0, yRef As Single = 0
            Single.TryParse(FeasaTools.FormatDecimal(txtRefx.Text), xRef)
            Single.TryParse(FeasaTools.FormatDecimal(txtRefy.Text), yRef)
            txtLog.Text = String.Concat(txtLog.Text, "xRef=" + xRef.ToString() + ", yRef=" + yRef.ToString(), vbCrLf)

            'Calibration
            For i = 1 To NFIBERS
                resp = FeasaCom.UserCal_AdjustxyOffsets(portNumber, i, xRef, yRef, toFlash)
                If (resp <> 1) Then
                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(portNumber)
                    Exit Sub
                End If
            Next


            'Check results
            FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes) 'Capture
            resp = FeasaCom.Send(portNumber, "GETxyALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub

            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Exit Sub
            End If

            txtLog.Text = String.Concat(txtLog.Text, Buffer.ToString(), vbCrLf)

            'Close the port
            FeasaCom.Close(portNumber)

        Else
            'Error unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub btnReadParams_Click(sender As Object, e As EventArgs) Handles btnReadParams.Click
        Dim portNumber As Integer
        Dim Buffer As New StringBuilder(1024)
        Dim i As Integer
        Dim NFIBERS As Integer
        Dim Gain As Integer
        Dim xOffset As Single, yOffset As Single
        Dim WavelengthOffset As Integer
        Dim AbsIntFactor As Double

        'Initialize variables
        NFIBERS = Convert.ToInt32(numFibers.Value)

        If (lstPorts.SelectedIndex = -1) Then
            Exit Sub
        End If

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), portNumber) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'Open port
        If FeasaCom.Open(portNumber, "57600") = 1 Then

            'No error

            'Retrieve Intensity gains
            For i = 1 To NFIBERS
                FeasaCom.UserCal_GetIntensityGain(portNumber, i, Gain)
                txtLog.Text = String.Concat(txtLog.Text, "Int Gain " + i.ToString("00") + ": " + Gain.ToString(), vbCrLf)
            Next

            'Retrieve xy Offsets
            For i = 1 To NFIBERS
                FeasaCom.UserCal_GetxyOffsets(portNumber, i, xOffset, yOffset)
                txtLog.Text = String.Concat(txtLog.Text, "xy Offsets " + i.ToString("00") + ": " + xOffset.ToString() + " " + yOffset.ToString(), vbCrLf)
            Next


            'Retrieve Wavelength Offsets
            For i = 1 To NFIBERS
                FeasaCom.UserCal_GetWavelengthOffset(portNumber, i, WavelengthOffset)
                txtLog.Text = String.Concat(txtLog.Text, "Wl Offsets " + i.ToString("00") + ": " + WavelengthOffset.ToString(), vbCrLf)
            Next


            'Retrieve Abs Int Factor
            For i = 1 To NFIBERS
                FeasaCom.UserCal_GetAbsIntFactor(portNumber, i, AbsIntFactor)
                txtLog.Text = String.Concat(txtLog.Text, "Abs Int Factor " + i.ToString("00") + ": " + AbsIntFactor.ToString(), vbCrLf)
            Next


            'Close the port
            FeasaCom.Close(portNumber)

        Else
            'Error unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub btnAdjustRGB_Click(sender As Object, e As EventArgs) Handles btnAdjustRGB.Click
        Dim portNumber As Integer
        Dim resp As Integer
        Dim Buffer As StringBuilder = New StringBuilder(1024)
        Dim i As Integer
        Dim c As Integer
        Dim NFIBERS As Integer
        Dim isPWM As Integer
        Dim CaptureRange As Integer
        Const PWMframes As Integer = 5

        'Initialize variables
        NFIBERS = Convert.ToInt32(numFibers.Value)
        If chkPWM.Checked Then isPWM = 1 Else isPWM = 0
        CaptureRange = lstCapture.SelectedIndex

        If lstPorts.SelectedIndex = -1 Then Exit Sub

        If lstCapture.SelectedIndex = -1 Then Exit Sub

        'Set the port number
        If Integer.TryParse(lstPorts.Items(lstPorts.SelectedIndex).ToString(), portNumber) = False Then Exit Sub

        'Clear the Results box
        txtLog.Clear()

        'Open port
        If FeasaCom.Open(portNumber, "57600") = 1 Then
            'No error

            '-------------------------------------------------
            ' RGB ADJUSTMENT
            '-------------------------------------------------

            'Reset
            For i = 1 To NFIBERS
                FeasaCom.UserCal_ResetRGBAdj(portNumber, i)
            Next i

            'Read back reference values
            Dim xRefR As Single = 0, yRefR As Single = 0 : Dim AbsIntRefR As Double = 0
            Dim xRefG As Single = 0, yRefG As Single = 0 : Dim AbsIntRefG As Double = 0
            Dim xRefB As Single = 0, yRefB As Single = 0 : Dim AbsIntRefB As Double = 0
            Single.TryParse(FormatDecimal(txtRefxR.Text), xRefR)
            Single.TryParse(FormatDecimal(txtRefyR.Text), yRefR)
            Double.TryParse(FormatDecimal(txtRefAbsIntR.Text), AbsIntRefR)
            Single.TryParse(FormatDecimal(txtRefxG.Text), xRefG)
            Single.TryParse(FormatDecimal(txtRefyG.Text), yRefG)
            Double.TryParse(FormatDecimal(txtRefAbsIntG.Text), AbsIntRefG)
            Single.TryParse(FormatDecimal(txtRefxB.Text), xRefB)
            Single.TryParse(FormatDecimal(txtRefyB.Text), yRefB)
            Double.TryParse(FormatDecimal(txtRefAbsIntB.Text), AbsIntRefB)
            txtLog.Text = String.Concat(txtLog.Text, "RED: xRef=" + xRefR.ToString() + ", yRef=" + yRefR.ToString() + ", AbsIntRef=" + AbsIntRefR.ToString(), vbCrLf)
            txtLog.Text = String.Concat(txtLog.Text, "GREEN: xRef=" + xRefG.ToString() + ", yRef=" + yRefG.ToString() + ", AbsIntRef=" + AbsIntRefG.ToString(), vbCrLf)
            txtLog.Text = String.Concat(txtLog.Text, "BLUE: xRef=" + xRefB.ToString() + ", yRef=" + yRefB.ToString() + ", AbsIntRef=" + AbsIntRefB.ToString(), vbCrLf)

            'Pre-cal
            Dim COLORS As String() = {"RED", "GREEN", "BLUE"}
            For c = 0 To 2

                If (MessageBox.Show("Please, switch on " & COLORS(c) & " LED and click OK to continue", "Color measurement", MessageBoxButtons.OKCancel, MessageBoxIcon.Information) = DialogResult.Cancel) Then Return

                'Capture
                resp = FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes)
                If (resp <> 1) Then

                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(portNumber)
                    Return
                End If

                For i = 1 To NFIBERS
                    'Store current measurements
                    resp = FeasaCom.UserCal_TakeRGBCurrentValues(portNumber, i, COLORS(c)(0))
                    If (resp <> 1) Then
                        FeasaCom.GetError_Description(Buffer)
                        MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                        FeasaCom.Close(portNumber)
                        Return
                    End If
                Next i 'FOR_FIBERS
            Next c 'FOR_COLORS

            'Adjustment
            For i = 1 To NFIBERS
                resp = FeasaCom.UserCal_AdjustRGB(portNumber, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB)
                If (resp <> 1) Then
                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(portNumber)
                    Return
                End If
            Next i

            'Capture
            resp = FeasaCom.Capture(portNumber, isPWM, CaptureRange, PWMframes)
            If (resp <> 1) Then

                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show("Error:" + Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Return
            End If
            'Check results
            resp = FeasaCom.Send(portNumber, "GETxyALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Return
            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Return
            End If
            txtLog.Text = String.Concat(txtLog.Text, Buffer.ToString(), vbCrLf)
            resp = FeasaCom.Send(portNumber, "GETABSINTALL", Buffer)
            If resp = -1 Then
                MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Return
            ElseIf resp = 0 Then
                MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(portNumber)
                Return
            End If
            txtLog.Text = String.Concat(txtLog.Text, Buffer.ToString(), vbCrLf)

            'Close the port
            FeasaCom.Close(portNumber)
        Else
            'Error: unable to open the selected port
            MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If
    End Sub
End Class
