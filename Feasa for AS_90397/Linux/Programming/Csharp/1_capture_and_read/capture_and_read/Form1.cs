/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture and Read
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser, perform a
*  measurement and download or read back the results
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

namespace capture_and_read
{
    public partial class Form1 : Form
    {

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Open(string DevPath, int Baudrate);

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Close(string DevPath);

        [DllImport("libfeasacom.so")]
		private static extern int FeasaCom_Send(string DevPath, string Command, StringBuilder ResponseText);

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
                resp = FeasaCom_Send(DevPath, "CAPTURE", buffer);
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

                //Shows the received data in the screen
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(),"\r\n");

                //Send command to the Led Analyser
                CommandToSend = String.Concat("00", txtFiber.Text); //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                CommandToSend = CommandToSend.Substring(CommandToSend.Length - 2);
                CommandToSend = String.Concat("GETRGBI", CommandToSend);
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(DevPath, CommandToSend, buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port","Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);
            }
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            //Add the port numbers to the list
            for (i = 0; i <= 10; i++)
            {
                lstPorts.Items.Add("/dev/ttyS" + i.ToString());
            }
			for (i = 0; i <= 10; i++)
			{
				lstPorts.Items.Add("/dev/ttyUSB" + i.ToString());
			}
            //Select the first port of the list
            lstPorts.SelectedIndex = 0;
        }
    }
}
