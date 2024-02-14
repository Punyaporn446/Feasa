/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (sweeping light)
*
*  DESCRIPTION: This example demonstrates how To use Sequence
*  functions provided In the DLL To test a sweeping light
*  pattern from different LEDs, extracting intensity and
*  pattern times afterwards.
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

namespace Sequence_sweeping
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

        }
        

        private void btnSequenceTest_Click(object sender, EventArgs e)
        {
            int DevicePort;
            int resp;
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
                int FibersToTest = (int)numFibers.Value;
                int StartDelay = (int)numStartDelay.Value;
                int CaptureTime = (int)numCaptureTime.Value;
                int WaitTime = (int)numWaitTime.Value;
                int SampleCount = (int)numSampleCount.Value;
                int isOffToOnPattern = chkIsOffToOnPattern.Checked ? 1 : 0;

                // Find out test settings
                resp = FeasaCom.Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                // Perform sequence capture
                resp = FeasaCom.Sequence_Capture(DevicePort, 0); //0: test all fibers
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }


                // Setup graph
                graphInt.GraphPane.CurveList.Clear();
                graphInt.GraphPane.Title.IsVisible = true;
                graphInt.GraphPane.Title.Text = "Intensity";
                graphInt.GraphPane.XAxis.Title.IsVisible = true;
                graphInt.GraphPane.XAxis.Title.Text = "Samples";
                graphInt.GraphPane.YAxis.Title.IsVisible = false;
                graphInt.GraphPane.Legend.IsVisible = false;
                graphInt.GraphPane.XAxis.Scale.Min = 0;
                graphInt.GraphPane.XAxis.Scale.Max = SampleCount - 1;
                graphInt.GraphPane.YAxis.Scale.Min = 0;

                IntensityValues = new int[SampleCount];

                double OffsetInt = 0;
                ZedGraph.LineItem[] gCourves = new ZedGraph.LineItem[FibersToTest];
                for (int f = 1; f <= FibersToTest; f++)
                {
                    // Read back results
                    resp = FeasaCom.Sequence_ReadIntensity(DevicePort, f, IntensityValues);
                    if (resp != 1)
                    {
                        FeasaCom.GetError_Description(buffer);
                        MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom.Close(DevicePort);
                        return;
                    }

                    // Plot intensity
                    ZedGraph.PointPairList gIntPoints = new ZedGraph.PointPairList();
                    for (int i = 0; i < SampleCount; i++)
                        gIntPoints.Add(i, (double)IntensityValues[i] + OffsetInt);
                    OffsetInt += (double)IntensityValues.Max() * 1.1;
                    graphInt.GraphPane.YAxis.Scale.Max = OffsetInt;
                    gCourves[f - 1] = graphInt.GraphPane.AddCurve("Fib " + f.ToString(), gIntPoints, Color.Blue, ZedGraph.SymbolType.None);
                } //FOR

                // Refresh graph
                graphInt.GraphPane.AxisChange();
                graphInt.Refresh();

                // Retrieve LED times
                int[] LowTimes = new int[FibersToTest];
                int[] HighTimes = new int[FibersToTest];
                int[] tIntensityValues = new int[FibersToTest];
                resp = FeasaCom.Sequence_GetSweepingPattern(DevicePort, FibersToTest, isOffToOnPattern, LowTimes, HighTimes, tIntensityValues);
                if (resp != 1)
                {
                    FeasaCom.GetError_Description(buffer);
                    MessageBox.Show(this, buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom.Close(DevicePort);
                    return;
                }

                // Print pattern times in grid
                datagridTimes.Rows.Clear();
                for (int i = 0; i < FibersToTest; i++)
                    datagridTimes.Rows.Add(i + 1, LowTimes[i], HighTimes[i], tIntensityValues[i]);
                datagridTimes.Refresh();

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
