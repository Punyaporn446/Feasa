/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture And Read (by Serial Number)
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser using the SN
*  instead of the COM port; then, perform a measurement and
*  download or read back the results
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
using System.Runtime.InteropServices;

namespace capture_and_read_by_sn
{
    public partial class Form1 : Form
    {

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_OpenSN(string SerialNumber, string Baudrate);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_CloseSN(string SerialNumber);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_SendSN(string SerialNumber, string Command, StringBuilder ResponseText);

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }

        private void cmdRead_Click(object sender, EventArgs e)
        {
            string SerialNumber;
            int resp;
            string CommandToSend;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Check the serial number
            if (txtSN.Text.Length < 4)
            {
                MessageBox.Show("Bad Serial Number", "Error", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            //Set the serial number
            SerialNumber = txtSN.Text;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            once your application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if (FeasaCom_OpenSN(SerialNumber, "57600") == 1)
            {
                //No error

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_SendSN(SerialNumber, "CAPTURE", buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_CloseSN(SerialNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_CloseSN(SerialNumber);
                    return;
                }

                //Shows the received data in the screen
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Send command to the LED Analyser
                CommandToSend = String.Concat("00", txtFiber.Text); //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                CommandToSend = CommandToSend.Substring(CommandToSend.Length - 2);
                CommandToSend = String.Concat("GETRGBI", CommandToSend);
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_SendSN(SerialNumber, CommandToSend, buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_CloseSN(SerialNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    FeasaCom_CloseSN(SerialNumber);
                    return;
                }

                //Shows the received data in the screen
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_CloseSN(SerialNumber);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }
    }
}
