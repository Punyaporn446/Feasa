'**************************************************************
'
'  (c) Feasa Enterprises Ltd
'  LED Analyser examples
'  Developed by: Carles Martínez Rius
'
'  PROJECT Sequence(sweeping light)
'
'  DESCRIPTION: This example demonstrates how To use Sequence
'  functions provided In the DLL To test a sweeping light
'  pattern from different LEDs, extracting intensity And
'  pattern times afterwards.
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

    End Sub

    Private Sub btnSequenceTest_Click(sender As Object, e As EventArgs) Handles btnSequenceTest.Click

        Dim DevicePort As Integer
        Dim resp As Integer
        Dim i As Integer
        Dim f As Integer
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
            Dim FibersToTest As Integer = numFibers.Value
            Dim StartDelay As Integer = numStartDelay.Value
            Dim CaptureTime As Integer = numCaptureTime.Value
            Dim WaitTime As Integer = numWaitTime.Value
            Dim SampleCount As Integer = numSampleCount.Value
            Dim isOffToOnPattern As Integer = 0

            If chkIsOffToOnPattern.Checked Then isOffToOnPattern = 1

            ' Set up test settings
            resp = FeasaCom.Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH)
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            ' Perform sequence capture
            resp = FeasaCom.Sequence_Capture(DevicePort, 0) '0: test all fibers
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            'Setup graph
            graphInt.GraphPane.CurveList.Clear()
            graphInt.GraphPane.Title.IsVisible = True
            graphInt.GraphPane.Title.Text = "Intensity"
            graphInt.GraphPane.XAxis.Title.IsVisible = True
            graphInt.GraphPane.XAxis.Title.Text = "Samples"
            graphInt.GraphPane.YAxis.Title.IsVisible = False
            graphInt.GraphPane.Legend.IsVisible = False
            graphInt.GraphPane.XAxis.Scale.Min = 0
            graphInt.GraphPane.XAxis.Scale.Max = SampleCount - 1
            graphInt.GraphPane.YAxis.Scale.Min = 0


            ReDim IntensityValues(SampleCount)

            Dim OffsetInt As Double = 0
            Dim gCourves(FibersToTest) As ZedGraph.LineItem
            For f = 1 To FibersToTest
                ' Read back results
                resp = FeasaCom.Sequence_ReadIntensity(DevicePort, f, IntensityValues)
                If (resp <> 1) Then
                    FeasaCom.GetError_Description(Buffer)
                    MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                    FeasaCom.Close(DevicePort)
                    Return
                End If

                ' Plot Intensity
                Dim gIntPoints = New ZedGraph.PointPairList()
                For i = 0 To SampleCount
                    gIntPoints.Add(i, IntensityValues(i) + OffsetInt)
                Next i
                OffsetInt += CDbl(IntensityValues.Max()) * 1.1
                graphInt.GraphPane.YAxis.Scale.Max = OffsetInt
                gCourves(f - 1) = graphInt.GraphPane.AddCurve("Fib " & i.ToString(), gIntPoints, Color.Blue, ZedGraph.SymbolType.None)
            Next f  'FOR

            ' Refresh graph
            graphInt.GraphPane.AxisChange()
            graphInt.Refresh()

            ' Retrieve LED times
            Dim LowTimes(FibersToTest) As Integer
            Dim HighTimes(FibersToTest) As Integer
            Dim tIntensityValues(FibersToTest) As Integer
            resp = FeasaCom.Sequence_GetSweepingPattern(DevicePort, FibersToTest, isOffToOnPattern, LowTimes, HighTimes, tIntensityValues)
            If (resp <> 1) Then
                FeasaCom.GetError_Description(Buffer)
                MessageBox.Show(Me, Buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                FeasaCom.Close(DevicePort)
                Return
            End If

            ' Print times in grid
            datagridTimes.Rows.Clear()
            For i = 0 To FibersToTest - 1
                datagridTimes.Rows.Add(i + 1, LowTimes(i), HighTimes(i), tIntensityValues(i))
            Next i
            datagridTimes.Refresh()

            ' Close port
            FeasaCom.Close(DevicePort)
        Else
            MessageBox.Show(Me, "Unable to open port!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

        Me.Enabled = True
        Me.Cursor = Cursors.Default

    End Sub

End Class
