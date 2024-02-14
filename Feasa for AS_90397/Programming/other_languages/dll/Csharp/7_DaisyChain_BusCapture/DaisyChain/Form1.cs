/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain (Bus capture)
*
*  DESCRIPTION: This example demonstrates how to perform
*  a capture for all Daisy-chained analysers, through the DLL
*  functions and then retrieve the HSI values for the fiber
*  requested.
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
***************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace DaisyChain
{
    public partial class Form1 : Form
    {

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Open(int CommPort, string Baudrate);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Close(int CommPort);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Send(int CommPort, string Command, StringBuilder ResponseText);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_IsPortAvailable(int CommPort);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_DaisyChain_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_DaisyChain_Add(int CommPort, string SerialNumber);

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            //List available ports
            for (i = 1; i <= 100; i++)
            {
                if (FeasaCom_IsPortAvailable(i) == 1) lstPorts.Items.Add(i.ToString());
            }
            //Select the first port of the list
            if (lstPorts.Items.Count > 0) lstPorts.SelectedIndex = 0;

            //Fill list with the Capture Mode
            lstCaptureMode.Items.Add("AUTO");
            lstCaptureMode.Items.Add("MANUAL");
            lstCaptureMode.SelectedIndex = 0;

            //Fill list with the Capture Ranges
            lstCaptureRange.Items.Add("LOW");
            lstCaptureRange.Items.Add("MEDIUM");
            lstCaptureRange.Items.Add("HIGH");
            lstCaptureRange.Items.Add("SUPER");
            lstCaptureRange.Items.Add("ULTRA");
            lstCaptureRange.SelectedIndex = 2;
        }

        private void cmdRead_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            portNumber = Convert.ToInt32(lstPorts.SelectedItem.ToString());

            //Check if Analysers has beed added to the Daisy-Chain list
            if (lstAnalysers.Items.Count == 0)
            {
                MessageBox.Show("Please, add Daisy-chained Analysers to the list first!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Get capture parameters
            int CaptureRange = 0;
            if (lstCaptureMode.SelectedIndex == 0)
            {
                CaptureRange = 0;
            }
            else if (lstCaptureMode.SelectedIndex == 1)
            {
                CaptureRange = lstCaptureRange.SelectedIndex + 1;
            }

            //Open port
            if (FeasaCom_Open(portNumber, "57600") == 1)
            {
                //Add Analysers to the daisy-chain bus
                for (int i = 0; i < lstAnalysers.Items.Count; i++)
                {
                    resp = FeasaCom_DaisyChain_Add(portNumber, lstAnalysers.Items[i].ToString());
                    if (resp == 0)
                    {
                        MessageBox.Show("Error! Unable to add Analyser to the bus!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }
                }

                //Perform a Bus capture
                resp = FeasaCom_DaisyChain_Capture(portNumber, 0, CaptureRange, 0);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }

                //Free BUS
                resp = FeasaCom_Send(portNumber, "BUSFREE", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }

                //Get Serial number of main analyser
                resp = FeasaCom_Send(portNumber, "GETSERIAL", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                string SN_main = buffer.ToString();

                //Get normalized fiber number
                string FibNum = numFiber.Value.ToString("00");

                //Get measurements from the Main analyser
                resp = FeasaCom_Send(portNumber, "GETHSI" + FibNum, buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }

                //Shows the received data in the screen
                txtLog.Text = String.Concat(txtLog.Text, "Fib " + FibNum + " (" + SN_main + "): " + buffer.ToString(), "\r\n");

                //Get measurements from Analysers attached to the BUS
                for (int i = 0; i < lstAnalysers.Items.Count; i++)
                {
                    //Set the bus owner
                    resp = FeasaCom_Send(portNumber, "BUSGET" + lstAnalysers.Items[i].ToString(), buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }

                    //Get measurements
                    resp = FeasaCom_Send(portNumber, "GETHSI" + FibNum, buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }

                    //Shows the received data in the screen
                    txtLog.Text = String.Concat(txtLog.Text, "Fib " + FibNum + " (" + lstAnalysers.Items[i].ToString() + "): " + buffer.ToString(), "\r\n");

                    //Free BUS
                    resp = FeasaCom_Send(portNumber, "BUSFREE", buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(portNumber);
                        return;
                    }
                }

                //Close the port
                FeasaCom_Close(portNumber);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void btnAddAnalyser_Click(object sender, EventArgs e)
        {
            if (txtSN.Text.Length != 4)
            {
                MessageBox.Show("Please, enter a valid Serial Number (4 characters)!", "Enter Serial Number", MessageBoxButtons.OK, MessageBoxIcon.Information);
                txtSN.Focus();
                return;
            }

            lstAnalysers.Items.Add(txtSN.Text);
            txtSN.Clear();
        }
    }
}
