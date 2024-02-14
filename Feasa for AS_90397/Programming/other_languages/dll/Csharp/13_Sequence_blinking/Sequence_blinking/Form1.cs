/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (blinking LED)
*
*  DESCRIPTION: This example demonstrates how to use Sequence
*  functions provided in the DLL to test a blinking LED
*  so that the light pattern could be tracked.
*
*  This example uses a dynamic library to access to the
*  functions related to the LED Analyser. This library is read
*  in runtime so you have to be sure that the file feasacom64.dll
*  exists in the same location of the EXE or in windows/system32
*  folder, however some compillers allow to reference the DLL
*  library from alternative locations using absolute or relative
*  paths.
*
*  Note: there are 32 and 64-bit versions of the DLL, so one or
*  the other has to be used depending on the compiler/IDE platform
*  or binary target platform.
*
**************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using FeasaLib;

namespace getRGBI
{
    public partial class Form1 : Form
    {
        private const int TOFLASH = 0;

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            once your application is running */
            //FeasaCom.EnumPorts();

            //List available ports
            for (i = 1; i <= 100; i++)
            {
                if (FeasaCom.IsPortAvailable(i)==1) lstPorts.Items.Add(i.ToString());
            }
            //Select the first port of the list
            if (lstPorts.Items.Count > 0) lstPorts.SelectedIndex = lstPorts.Items.Count - 1;

            //Add options to list
            lstSignalSpeed.Items.Clear();
            lstSignalSpeed.Items.Add("VERY LOW (<1Hz)");
            lstSignalSpeed.Items.Add("LOW (1-3Hz)");
            lstSignalSpeed.Items.Add("MEDIUM (3-10Hz)");
            lstSignalSpeed.Items.Add("MODERATE (10-20Hz)");
            lstSignalSpeed.Items.Add("HIGH (20-40Hz)");
            lstSignalSpeed.Items.Add("VERY HIGH (>40Hz)");
            lstSignalSpeed.SelectedIndex = 3;

            lstBlinkingSpeed.Items.Clear();
            lstBlinkingSpeed.Items.Add("0: VERY LOW");
            lstBlinkingSpeed.Items.Add("1: VERY LOW");
            lstBlinkingSpeed.Items.Add("2: LOW");
            lstBlinkingSpeed.Items.Add("3: LOW");
            lstBlinkingSpeed.Items.Add("4: MEDIUM");
            lstBlinkingSpeed.Items.Add("5: MEDIUM");
            lstBlinkingSpeed.Items.Add("6: MODERATE (fast blinking)");
            lstBlinkingSpeed.Items.Add("7: MODERATE (very fast blinking)");
            lstBlinkingSpeed.Items.Add("8: HIGH (can barely see it)");
            lstBlinkingSpeed.Items.Add("9: HIGH");
            lstBlinkingSpeed.Items.Add("10: VERY HIGH (can't see it)");
            lstBlinkingSpeed.SelectedIndex = 6;

        }

        private void btnFindParams_Click(object sender, EventArgs e)
        {
            int DevicePort;
            int resp;
            StringBuilder buffer = new StringBuilder(255);

            //Check if port is selected
            if (lstPorts.SelectedIndex == -1)
            {
                MessageBox.Show(this, "Please, select a port from the list.", "COM port not selected", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }
            DevicePort = Convert.ToInt32(lstPorts.Items[lstPorts.SelectedIndex]);

            //Disable form
            this.Enabled = false;
            this.Cursor = Cursors.WaitCursor;

            //Increase maximum timeout to avoid errors caused by long captures
            FeasaCom.SetResponseTimeout(8000); //8000 milliseconds

            if (FeasaCom.Open(DevicePort, "AUTO") == 1)
            {
                //Retrieve test data
                int SignalSpeed = lstSignalSpeed.SelectedIndex * 2;
                int BlinkingSpeed = lstBlinkingSpeed.SelectedIndex;
                int MinCycleCount = (int)numCycles.Value;
                int TimeResolutionIsImportant = chkTimeResImportant.Checked ? 1 : 0;
                int TotalLEDCount = (int)numLEDCount.Value;
                int Fiber = (int)numFiberToTest.Value;
                int CaptureTime = 0;
                int WaitTime = 0;
                int SampleCount = 0;

                // Find out test settings
                resp = FeasaCom.Sequence_FindTestSettings(DevicePort, TotalLEDCount, Fiber, SignalSpeed, BlinkingSpeed, MinCycleCount, TimeResolutionIsImportant, ref CaptureTime, ref WaitTime, ref SampleCount);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                // Sample count
                numCaptureTime.Value = CaptureTime;
                numWaitTime.Value = WaitTime;
                numSampleCount.Value = SampleCount;

                //Close port
                FeasaCom.Close(DevicePort);

                MessageBox.Show(this, "Parameters calculated successfully", "Succeeded", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            else
            {
                MessageBox.Show(this, "Unable to open port!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

            this.Enabled = true;
            this.Cursor = Cursors.Default;
        }

        private void btnSequenceTest_Click(object sender, EventArgs e)
        {
            int DevicePort;
            int resp;
            float[] xValues;
            float[] yValues;
            int[] IntensityValues;
            StringBuilder buffer = new StringBuilder(255);

            //Check if port is selected
            if (lstPorts.SelectedIndex == -1)
            {
                MessageBox.Show(this, "Please, select a port from the list.", "COM port not selected", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }
            DevicePort = Convert.ToInt32(lstPorts.Items[lstPorts.SelectedIndex]);

            //Disable form
            this.Enabled = false;
            this.Cursor = Cursors.WaitCursor;

            //Increase maximum timeout to avoid errors caused by long captures
            FeasaCom.SetResponseTimeout(8000); //8000 milliseconds

            if (FeasaCom.Open(DevicePort, "AUTO") == 1)
            {
                //Retrieve test data
                int Fiber = (int)numFiber.Value;
                int StartDelay = (int)numStartDelay.Value;
                int CaptureTime = (int)numCaptureTime.Value;
                int WaitTime = (int)numWaitTime.Value;
                int SampleCount = (int)numSampleCount.Value;

                // Set up sequence
                resp = FeasaCom.Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                // Perform sequence capture
                resp = FeasaCom.Sequence_Capture(DevicePort, Fiber);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                // Read back results
                xValues = new float[SampleCount];
                yValues = new float[SampleCount];
                IntensityValues = new int[SampleCount];
                resp = FeasaCom.Sequence_ReadxyI(DevicePort, Fiber, xValues, yValues, IntensityValues);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                //Plot Intensity
                graphInt.GraphPane.CurveList.Clear();
                graphInt.GraphPane.Title.IsVisible = true;
                graphInt.GraphPane.Title.Text = "Intensity";
                graphInt.GraphPane.XAxis.Title.IsVisible = true;
                graphInt.GraphPane.XAxis.Title.Text = "Samples";
                graphInt.GraphPane.YAxis.Title.IsVisible = false;
                graphInt.GraphPane.Legend.IsVisible = false;
                ZedGraph.PointPairList gIntPoints = new ZedGraph.PointPairList();
                int MaxIntensity = 0;
                for (int i = 0; i < SampleCount; i++)
                {
                    gIntPoints.Add(i, (double)IntensityValues[i]);
                    if (IntensityValues[i] > MaxIntensity) MaxIntensity = IntensityValues[i];
                }
                graphInt.GraphPane.XAxis.Scale.Min = 0;
                graphInt.GraphPane.XAxis.Scale.Max = SampleCount - 1;
                graphInt.GraphPane.YAxis.Scale.Min = 0;
                graphInt.GraphPane.YAxis.Scale.Max = (double)MaxIntensity * 1.1;
                ZedGraph.LineItem myCurve = graphInt.GraphPane.AddCurve("Intensity", gIntPoints, Color.Blue, ZedGraph.SymbolType.None);
                graphInt.GraphPane.AxisChange();
                graphInt.Refresh();

                //Plot CIE
                graphCIE.GraphPane.CurveList.Clear();
                graphCIE.GraphPane.Title.IsVisible = true;
                graphCIE.GraphPane.Title.Text = "CIE1931";
                graphCIE.GraphPane.XAxis.Title.IsVisible = true;
                graphCIE.GraphPane.XAxis.Title.Text = "Samples";
                graphCIE.GraphPane.YAxis.Title.IsVisible = false;
                graphCIE.GraphPane.Legend.IsVisible = false;
                ZedGraph.PointPairList gxPoints = new ZedGraph.PointPairList();
                ZedGraph.PointPairList gyPoints = new ZedGraph.PointPairList();
                for (int i = 0; i < SampleCount; i++)
                {
                    gxPoints.Add(i, (double)xValues[i]);
                    gyPoints.Add(i, (double)yValues[i]);
                }
                graphCIE.GraphPane.XAxis.Scale.Min = 0;
                graphCIE.GraphPane.XAxis.Scale.Max = SampleCount - 1;
                graphCIE.GraphPane.YAxis.Scale.Min = 0;
                graphCIE.GraphPane.YAxis.Scale.Max = 1;
                ZedGraph.LineItem gCIEx= graphCIE.GraphPane.AddCurve("x", gxPoints, Color.Red, ZedGraph.SymbolType.None);
                ZedGraph.LineItem gCIEy = graphCIE.GraphPane.AddCurve("y", gyPoints, Color.Green, ZedGraph.SymbolType.None);
                graphCIE.GraphPane.AxisChange();
                graphCIE.Refresh();

                //Close port
                FeasaCom.Close(DevicePort);
            }
            else
            {
                MessageBox.Show(this, "Unable to open port!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

            this.Enabled = true;
            this.Cursor = Cursors.Default;
        }
    }
}
