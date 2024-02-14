/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Fast Test (Multi-threaded)
*
*  DESCRIPTION: This example demonstrates how to use the multi-
*  threaded functions provided in the DLL to set up a fast and
*  efficient communication schema for your application.
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
            if (lstPorts.Items.Count>0) lstPorts.SelectedIndex = 0;

            //Setup data grid
            dataGrid.RowCount = 20;
            //Initialize grid values
            for (i = 0; i < 19; i++)
            {
                dataGrid.Rows[i].Cells[0].Value = 0;
                dataGrid.Rows[i].Cells[1].Value = 0;
                dataGrid.Rows[i].Cells[2].Value = 0;
                dataGrid.Rows[i].Cells[3].Value = 0;
            }
        }

        private void cmdCapture_Click(object sender, EventArgs e)
        {
            int resp;
            StringBuilder buffer = new StringBuilder(100);
            int PortCount = 0;
            DateTime dtIni = DateTime.Now;

            if (lstPortsToTest.Items.Count == 0)
            {
                MessageBox.Show(this, "Please, add at least one port to the list of devices to be tested.", "No ports added", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            //Get the number of ports added to the list
            PortCount = lstPortsToTest.Items.Count;

            //Initialize variables
            int[] ReturnValues = new int[PortCount];
            int[] Ports = new int[PortCount];
            string[] Responses = new string[PortCount];
            string[] SNs = new string[PortCount];

            for (int i = 0; i < PortCount; i++)
                Ports[i] = Convert.ToInt32(lstPortsToTest.Items[i]);

            //Open port
            if (FeasaCom.Open_Multi(ReturnValues, Ports, Ports.Length, "57600") == 1)
            {
                //No error

                //Retrieve Serial numbers connected
                for (int i = 0; i < Ports.Length; i++)
                {
                    resp = FeasaCom.GetSNByPort(buffer, Ports[i]);
                    SNs[i] = (resp == 1) ? buffer.ToString() : "";
                }

                //Initialize string for responses (necessary to allocate space)
                // Has to be done just before receiving any response
                FeasaTools.InitializeArrayOfStrings(ref Responses, 100);

                //Send command to All Analysers connected
                resp = FeasaCom.SendToAll(ReturnValues, "CAPTURE", Responses);
                if (resp != 1)
                {
                    for (int i = 0; i < PortCount; i++)
                        if (ReturnValues[i] == -1)
                        {
                            MessageBox.Show("Unable to send the command to " + SNs[i] + "!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length);
                            return;
                        }
                        else if (ReturnValues[i] == 0)
                        {
                            MessageBox.Show("Timeout or Syntax error detected in " + SNs[i] + "!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length);
                            return;
                        }
                }

                //Initialize string for responses (necessary to allocate space)
                // Has to be done just before receiving any response
                FeasaTools.InitializeArrayOfStrings(ref Responses, 500);

                //Send command to All Analysers connected
                resp = FeasaCom.SendToAll(ReturnValues, "GETHSIALL", Responses);
                if (resp != 1)
                {
                    for (int i = 0; i < PortCount; i++)
                        if (ReturnValues[i] == -1)
                        {
                            MessageBox.Show("Unable to send the command to " + SNs[i] + "!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length);
                            return;
                        }
                        else if (ReturnValues[i] == 0)
                        {
                            MessageBox.Show("Timeout or Syntax error detected in " + SNs[i] + "!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length);
                            return;
                        }
                }

                //Clear grid
                dataGrid.Rows.Clear();

                //Extract response lines and parse responses
                for (int i = 0; i < PortCount; i++)
                {
                    //Declare numerical arrays for responses with 20 elements (maximum number of channels)
                    float[] HueValues = new float[20];
                    int[] SaturationValues = new int[20];
                    int[] IntensityValues = new int[20];
                    int nLines = FeasaTools.Parse_HSI_All(Responses[i], HueValues, SaturationValues, IntensityValues);
                    if (nLines > 0)
                    {
                        for (int f = 0; f < nLines; f++)
                            dataGrid.Rows.Add(SNs[i], f + 1, HueValues[f], SaturationValues[f], IntensityValues[f]);
                    }
                }

                //Close the port
                FeasaCom.Close_Multi(ReturnValues, Ports, Ports.Length);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open all ports", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

            //Calculates execution time
            TimeSpan ts = DateTime.Now - dtIni;
            lblExecutionTime.Text = "Execution time: " + ts.TotalMilliseconds + " ms";
        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            if (lstPorts.SelectedIndex>=0)
            {
                //Adds selected port to the list of port to be tested
                lstPortsToTest.Items.Add(lstPorts.Items[lstPorts.SelectedIndex]);
                //Removes port from list of ports
                lstPorts.Items.RemoveAt(lstPorts.SelectedIndex);
            }
        }
    }
}
