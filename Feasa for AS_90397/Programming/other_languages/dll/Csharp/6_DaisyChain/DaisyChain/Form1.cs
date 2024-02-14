/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with a daisy chained Feasa LED Analyser
*  using commands GETBUS and FREEBUS.
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

        public Form1()
        {
            InitializeComponent();
        }

        private void cmdRead_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            string CommandToSend;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            portNumber = lstPorts.SelectedIndex + 1;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if (FeasaCom_Open(portNumber, "57600") == 1)
            {
                //No error

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, "CAPTURE", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Send command to the LED Analyser
                CommandToSend = String.Concat("00", txtFiber.Text); //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                CommandToSend = CommandToSend.Substring(CommandToSend.Length - 2);
                CommandToSend = String.Concat("GETRGBI", CommandToSend);
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, CommandToSend, buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(portNumber);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void cmdGet_Click(object sender, EventArgs e)
        {
            int portNumber;
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
            portNumber = lstPorts.SelectedIndex + 1;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if ( FeasaCom_Open(portNumber, "57600") == 1 )
            {
                //No error

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, String.Concat("BUSGET",SN), buffer);
                if (resp == -1)
                {
                    MessageBox.Show("Error! unable to send the command!","Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else if (resp == 0)
                {
                    MessageBox.Show("Timeout detected!","Error",MessageBoxButtons.OK,MessageBoxIcon.Exclamation);
                    FeasaCom_Close(portNumber);
                    return;
                }
                else
                {
                    //Shows info text
                    txtLog.Text = String.Concat(buffer.ToString(), "\r\n", "The Bus belongs to the LED Analyser ", SN);
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

        private void cmdFree_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            string CommandToSend;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            txtLog.Clear();

            //Set the port number
            portNumber = lstPorts.SelectedIndex + 1;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if (FeasaCom_Open(portNumber, "57600") == 1)
            {
                //No error

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
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
                else
                {
                    //Shows info text
                    txtLog.Text = String.Concat(buffer.ToString(), "\r\n", "The bus is Free!");
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

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            //List available ports
            for (i = 1; i <= 100; i++)
            {
                lstPorts.Items.Add(i.ToString());
            }
            //Select the first port of the list
            lstPorts.SelectedIndex = 0;
        }
    }
}
