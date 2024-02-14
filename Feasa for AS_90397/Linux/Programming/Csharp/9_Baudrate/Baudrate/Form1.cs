﻿/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: baudrate
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser picking a
*  known baudrate or detecting it automatically.
*
*  Important Note: it is not possible to communicate to a
*  Led Analyser that does not have axactly the same
*  baudrate used to open the port.
*  Factory default: 57600 baud.
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

namespace Baudrate
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
		private static extern int FeasaCom_GetBaudrate(string DevPath);

		[DllImport("libfeasacom.so")]
		private static extern int FeasaCom_IsPortAvailable(string DevPath);
        
        public Form1()
        {
            InitializeComponent();
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
            lstPorts.SelectedIndex = 0;

            //Add the possible Baudrates
            lstBaudrate.Items.Add(9600);
            lstBaudrate.Items.Add(19200);
            lstBaudrate.Items.Add(38400);
            lstBaudrate.Items.Add(57600);
            lstBaudrate.Items.Add(115200);
            lstBaudrate.Items.Add(230400);
            lstBaudrate.Items.Add(460800);
            lstBaudrate.Items.Add(921600);
            lstBaudrate.SelectedIndex = 3;
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
            if (optBaudrateAuto.Checked == true)
            {
                resp = FeasaCom_Open(DevPath, 0);
            }
            else
            {
				resp = FeasaCom_Open(DevPath, Convert.ToInt32(lstBaudrate.SelectedItem));
            }

            //Open port
            if (resp == 1)
            {
                //No error

                //Send command to the Led Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(DevPath, "CAPTURE", buffer);
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

                //Get the baudrate detected
                if (optBaudrateAuto.Checked)
                {
                    txtLog.Text = String.Concat(txtLog.Text, "Baudrate: " + FeasaCom_GetBaudrate(DevPath).ToString(), "\r\n");
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

        private void optBaudrateAuto_CheckedChanged(object sender, EventArgs e)
        {
            lstBaudrate.Enabled = false;
        }

        private void optBaudrateManual_CheckedChanged(object sender, EventArgs e)
        {
            lstBaudrate.Enabled = true;
        }

    }
}
