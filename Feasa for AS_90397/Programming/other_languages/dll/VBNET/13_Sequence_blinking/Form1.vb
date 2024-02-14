'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT: Sequence (blinking LED)
'
'  DESCRIPTION: This example demonstrates how To use Sequence
'  functions provided In the DLL To test a blinking LED
'  so that the light pattern could be tracked.
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

    Private Const TOFLASH As Integer = 0

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
        lstPorts.SelectedIndex = lstPorts.Items.Count - 1

        'Add options to list
        lstSignalSpeed.Items.Clear()
        lstSignalSpeed.Items.Add("VERY LOW (<1Hz)")
        lstSignalSpeed.Items.Add("LOW (1-3Hz)")
        lstSignalSpeed.Items.Add("MEDIUM (3-10Hz)")
        lstSignalSpeed.Items.Add("MODERATE (10-20Hz)")
        lstSignalSpeed.Items.Add("HIGH (20-40Hz)")
        lstSignalSpeed.Items.Add("VERY HIGH (>40Hz)")
        lstSignalSpeed.SelectedIndex = 3

        lstBlinkingSpeed.Items.Clear()
        lstBlinkingSpeed.Items.Add("0: VERY LOW")
        lstBlinkingSpeed.Items.Add("1: VERY LOW")
        lstBlinkingSpeed.Items.Add("2: LOW")
        lstBlinkingSpeed.Items.Add("3: LOW")
        lstBlinkingSpeed.Items.Add("4: MEDIUM")
        lstBlinkingSpeed.Items.Add("5: MEDIUM")
        lstBlinkingSpeed.Items.Add("6: MODERATE (fast blinking)")
        lstBlinkingSpeed.Items.Add("7: MODERATE (very fast blinking)")
        lstBlinkingSpeed.Items.Add("8: HIGH (can barely see it)")
        lstBlinkingSpeed.Items.Add("9: HIGH")
        lstBlinkingSpeed.Items.Add("10: VERY HIGH (can't see it)")
        lstBlinkingSpeed.SelectedIndex = 6

    End Sub

    Private Sub btnFindParams_Click(sender As Object, e As EventArgs) Handles btnFindParams.Click
        Dim DevicePort As Integer
        Dim resp As Integer
        Dim Buffer = New StringBuilder(255)

        'Check if port Is selected
        If lstPorts.SelectedIndex = -1 Then
            MessageBox.Show(Me, "Please, select a port from the list.", "COM port not selected", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            Exit Sub
        End If
        DevicePort = Convert.ToInt32(lstPorts.Items(lstPorts.SelectedIndex))

        'Disable form
        Me.Enabled = False
        Me.Cursor = Cursors.WaitCursor

        'Increase maximum timeout to avoid errors caused by long captures
        FeasaCom.SetResponseTimeout(8000) '8000 milliseconds

        If (FeasaCom.Open(DevicePort, "AUTO") = 1) Then

            'Retrieve test data
            Dim SignalSpeed = lstSignalSpeed.SelectedIndex * 2
            Dim BlinkingSpeed As Integer = lstBlinkingSpeed.SelectedIndex
            Dim MinCycleCount As Integer = numCycles.Value
            Dim TimeResolutionIsImportant As Integer
            Dim TotalLEDCount As Integer = numLEDCount.Value
            Dim Fiber As Integer = numFiberToTest.Value
            Dim CaptureTime As Integer = 0
            Dim WaitTime As Integer = 0
            Dim SampleCount As Integer = 0

            If chkTimeResImportant.Checked Then TimeResolutionIsImportant = 1 Else TimeResolutionIsImportant = 0

            ' Find out test settings
            resp = FeasaCom.Sequence_FindTestSettings(DevicePort, TotalLEDCount, Fiber, SignalSpeed, BlinkingSpeed, MinCycleCount, TimeResolutionIsImportant, CaptureTime, WaitTime, SampleCount)
            If (resp <> 1) Then

                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Exit Sub
            End If

            ' Sample count
            numCaptureTime.Value = CaptureTime
            numWaitTime.Value = WaitTime
            numSampleCount.Value = SampleCount

            'Close port
            FeasaCom.Close(DevicePort)

            MessageBox.Show(Me, "Parameters calculated successfully", "Succeeded", MessageBoxButtons.OK, MessageBoxIcon.Information)

        Else
            MessageBox.Show(Me, "Unable to open port!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

        Me.Enabled = True
        Me.Cursor = Cursors.Default
    End Sub

    Private Sub btnSequenceTest_Click(sender As Object, e As EventArgs) Handles btnSequenceTest.Click

        Dim DevicePort As Integer
        Dim resp As Integer
        Dim i As Integer
        Dim xValues() As Single
        Dim yValues() As Single
        Dim IntensityValues() As Integer
        Dim Buffer = New StringBuilder(255)

        'Check if port Is selected
        If (lstPorts.SelectedIndex = -1) Then
            MessageBox.Show(Me, "Please, select a port from the list.", "COM port not selected", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            Return
        End If
        DevicePort = Convert.ToInt32(lstPorts.Items(lstPorts.SelectedIndex))

        'Disable form
        Me.Enabled = False
        Me.Cursor = Cursors.WaitCursor

        'Increase maximum timeout to avoid errors caused by long captures
        FeasaCom.SetResponseTimeout(8000) '8000 milliseconds

        If (FeasaCom.Open(DevicePort, "AUTO") = 1) Then

            'Retrieve test data
            Dim Fiber As Integer = numFiber.Value
            Dim StartDelay As Integer = numStartDelay.Value
            Dim CaptureTime As Integer = numCaptureTime.Value
            Dim WaitTime As Integer = numWaitTime.Value
            Dim SampleCount As Integer = numSampleCount.Value

            ' Set up test settings
            resp = FeasaCom.Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH)
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            ' Perform sequence capture
            resp = FeasaCom.Sequence_Capture(DevicePort, Fiber)
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            ' Read back results
            ReDim xValues(SampleCount)
            ReDim yValues(SampleCount)
            ReDim IntensityValues(SampleCount)
            resp = FeasaCom.Sequence_ReadxyI(DevicePort, Fiber, xValues, yValues, IntensityValues)
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            'Plot Intensity
            graphInt.GraphPane.CurveList.Clear()
            graphInt.GraphPane.Title.IsVisible = True
            graphInt.GraphPane.Title.Text = "Intensity"
            graphInt.GraphPane.XAxis.Title.IsVisible = True
            graphInt.GraphPane.XAxis.Title.Text = "Samples"
            graphInt.GraphPane.YAxis.Title.IsVisible = False
            graphInt.GraphPane.Legend.IsVisible = False
            Dim gIntPoints = New ZedGraph.PointPairList()
            Dim MaxIntensity As Integer = 0
            For i = 0 To SampleCount
                gIntPoints.Add(i, IntensityValues(i))
                If (IntensityValues(i) > MaxIntensity) Then MaxIntensity = IntensityValues(i)
            Next i
            graphInt.GraphPane.XAxis.Scale.Min = 0
            graphInt.GraphPane.XAxis.Scale.Max = SampleCount - 1
            graphInt.GraphPane.YAxis.Scale.Min = 0
            graphInt.GraphPane.YAxis.Scale.Max = MaxIntensity * 1.1
            Dim myCurve = graphInt.GraphPane.AddCurve("Intensity", gIntPoints, Color.Blue, ZedGraph.SymbolType.None)
            graphInt.GraphPane.AxisChange()
            graphInt.Refresh()

            'Plot CIE
            graphCIE.GraphPane.CurveList.Clear()
            graphCIE.GraphPane.Title.IsVisible = True
            graphCIE.GraphPane.Title.Text = "CIE1931"
            graphCIE.GraphPane.XAxis.Title.IsVisible = True
            graphCIE.GraphPane.XAxis.Title.Text = "Samples"
            graphCIE.GraphPane.YAxis.Title.IsVisible = False
            graphCIE.GraphPane.Legend.IsVisible = False
            Dim gxPoints = New ZedGraph.PointPairList()
            Dim gyPoints = New ZedGraph.PointPairList()
            For i = 0 To SampleCount
                gxPoints.Add(i, xValues(i))
                gyPoints.Add(i, yValues(i))
            Next i
            graphCIE.GraphPane.XAxis.Scale.Min = 0
            graphCIE.GraphPane.XAxis.Scale.Max = SampleCount - 1
            graphCIE.GraphPane.YAxis.Scale.Min = 0
            graphCIE.GraphPane.YAxis.Scale.Max = 1
            Dim gCIEx = graphCIE.GraphPane.AddCurve("x", gxPoints, Color.Red, ZedGraph.SymbolType.None)
            Dim gCIEy = graphCIE.GraphPane.AddCurve("y", gyPoints, Color.Green, ZedGraph.SymbolType.None)
            graphCIE.GraphPane.AxisChange()
            graphCIE.Refresh()

            'Close port
            FeasaCom.Close(DevicePort)
        Else
            MessageBox.Show(Me, "Unable to open port!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

        Me.Enabled = True
        Me.Cursor = Cursors.Default
    End Sub
End Class
