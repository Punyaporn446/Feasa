/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain (Bus capture)
*
*  DESCRIPTION: This example demonstrates how to perform
*  a capture for all Daisy-chained analysers, through the
*  SO Library functions and then retrieve the HSI values for
*  the fiber requested.
*
*  This example uses a dynamic library to access to the
*  functions related to the LED Analyser. This library is read
*  in runtime, so you have to be sure that the file
*  libfeasacom_x86_64.so has been copied to the /usr/lib/
*  directory or equivalent, moreover,  some compillers/IDE
*  allow to reference the SO library from the same location
*  of the binary/script or alternative locations using absolute
*  or relative paths.
*
*  Note: there are 32 and 64-bit versions of the SO Library, so
*  one or the other has to be used depending on the compiler/IDE
*  platform or binary target platform.
*
***************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace DaisyChain
{
    public partial class Form1 : Form
    {

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Open(string DevPath, int Baudrate);

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Close(string DevPath);

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Send(string DevPath, string Command, StringBuilder ResponseText);

        [DllImport("libfeasacom.so")]
        private static extern int FeasaCom_IsPortAvailable(string DevPath);

        [DllImport("libfeasacom.so")]
        private static extern int FeasaCom_DaisyChain_Capture(string DevPath, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport("libfeasacom.so")]
        private static extern int FeasaCom_DaisyChain_Add(string DevPath, string SerialNumber);

        public Form1()
        {
            InitializeComponent();
        }

        private void cmdRead_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            string CommandToSend;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

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
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //Add Analysers to the daisy-chain bus
                for (int i = 0; i < lstAnalysers.Items.Count; i++)
                {
                    resp = FeasaCom_DaisyChain_Add(DevPath, lstAnalysers.Items[i].ToString());
                    if (resp == 0)
                    {
                        MessageBox.Show("Error! Unable to add Analyser to the bus!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Perform a Bus capture
                resp = FeasaCom_DaisyChain_Capture(DevPath, 0, CaptureRange, 0);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }

                //Free BUS
                resp = FeasaCom_Send(DevPath, "BUSFREE", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }

                //Get Serial number of main analyser
                resp = FeasaCom_Send(DevPath, "GETSERIAL", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                string SN_main = buffer.ToString();

                //Get normalized fiber number
                string FibNum = numFiber.Value.ToString("00");

                //Get measurements from the Main analyser
                resp = FeasaCom_Send(DevPath, "GETHSI" + FibNum, buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }

                //Shows the received data in the screen
                txtLog.Text = String.Concat(txtLog.Text, "Fib " + FibNum + " (" + SN_main + "): " + buffer.ToString(), "\r\n");

                //Get measurements from Analysers attached to the BUS
                for (int i = 0; i < lstAnalysers.Items.Count; i++)
                {
                    //Set the bus owner
                    resp = FeasaCom_Send(DevPath, "BUSGET" + lstAnalysers.Items[i].ToString(), buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }

                    //Get measurements
                    resp = FeasaCom_Send(DevPath, "GETHSI" + FibNum, buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }

                    //Shows the received data in the screen
                    txtLog.Text = String.Concat(txtLog.Text, "Fib " + FibNum + " (" + lstAnalysers.Items[i].ToString() + "): " + buffer.ToString(), "\r\n");

                    //Free BUS
                    resp = FeasaCom_Send(DevPath, "BUSFREE", buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void cmdGet_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            string SN;
            StringBuilder buffer = new StringBuilder(100);

            //Check SN
            if (txtSN.Text.Length < 4)
            {
                MessageBox.Show("Bad Serial Number","Error", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            //Get the serial number
            SN = String.Concat("0000",txtSN.Text); //Force a 4-digits format
            SN = SN.Substring(SN.Length - 4);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if ( FeasaCom_Open(DevPath, 57600) == 1 )
            {
                //No error

                //Send command to the Led Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(DevPath, String.Concat("BUSGET",SN), buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!","Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!","Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else
                {
                    //Shows info text
                    txtLog.Text = String.Concat(buffer.ToString(), "\r\n", "The Bus belongs to the Led Analyser ", SN);
                }

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void cmdFree_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            string CommandToSend;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //Send command to the Led Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(DevPath, "BUSFREE", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_Close(DevPath);
                    return;
                }
                else
                {
                    //Shows info text
                    txtLog.Text = String.Concat(buffer.ToString(), "\r\n", "The bus is Free!");
                }

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            //Add the port numbers in the list from 0 to 16
			for (i = 0; i <= 10; i++)
			{
				if (FeasaCom_IsPortAvailable("/dev/ttyS" + i.ToString())==1) lstPorts.Items.Add("/dev/ttyS" + i.ToString());
			}
			for (i = 0; i <= 10; i++)
			{
				if (FeasaCom_IsPortAvailable("/dev/ttyUSB" + i.ToString())==1) lstPorts.Items.Add("/dev/ttyUSB" + i.ToString());
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
