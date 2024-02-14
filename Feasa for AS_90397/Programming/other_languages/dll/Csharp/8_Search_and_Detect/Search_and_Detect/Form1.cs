/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Search & Detect
*
*  DESCRIPTION: This example demonstrates how to list
*  all available Feasa devices, and also to locate
*  the port number of a connected Device based on its serial
*  number.
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

namespace Search_LedAnalyser
{
    public partial class Form1 : Form
    {

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_EnumPorts();

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Open(int CommPort, string Baudrate);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Close(int CommPort);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Send(int CommPort, string Command, StringBuilder ResponseText);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_IsConnected(string SerialNumber, string Baudrate);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_SetResponseTimeout(uint Timeout);

        [DllImport("feasacom64.dll")]
        public static extern int FeasaCom_Detect(int[] CommPorts, string Baudrate);

        [DllImport("feasacom64.dll")]
        public static extern int FeasaCom_DetectSN([In, Out, MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPStr)] string[] SerialNumbers, string Baudrate);

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }

        private void cmdSearch_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            StringBuilder buffer = new StringBuilder(1048);
            string SerialNumber;

            //Check SN
            if (txtSN.Text.Length < 4)
            {
                MessageBox.Show("Bad Serial Number","Error", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return;
            }

            //Get the serial number
            SerialNumber = String.Concat("0000", txtSN.Text); //Force a 4-digits format
            SerialNumber = SerialNumber.Substring(SerialNumber.Length - 4);

            //Clear the Results box
            txtLog.Clear();

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Log
            txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

            //Find out if the LED Analyser is connected
            portNumber = FeasaCom_IsConnected(SerialNumber, "57600");
            if ( portNumber == -1 )
            {
                txtLog.Text = String.Concat(txtLog.Text, "The LED Analyser with the SN:", SerialNumber, " was not found", "\r\n");
                return;
            }
            else
            {
                //Log
                txtLog.Text = String.Concat(txtLog.Text, "LED Analyser found on port ", portNumber.ToString(),  "\r\n");
            }

            //Open port
            if (FeasaCom_Open(portNumber, "57600") == 1)
            {
                //No error

                //Setup special conditions for "long response commands"
                FeasaCom_SetResponseTimeout(150); //timeout to 500 ms

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, "GETSTATUS", buffer);
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
                    //Shows the received data in the screen
                    txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");
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

        private void btnDetect_Click(object sender, EventArgs e)
        {
            int[] Ports = new int[255];

            //Clear the Results box
            txtLog.Clear();

            //Re-enumerate ports (in case anything has been plugged or unplugged)
            FeasaCom_EnumPorts();

            //Perform detection
            int nDetected = FeasaCom_Detect(Ports, "AUTO");
            if (nDetected > 0)
            {
                //List devices detected
                string aux = "Devices detected:\r\n";
                for (int i = 0; i < nDetected; i++)
                    aux += "...on port COM" + Ports[i] + "\r\n";
                txtLog.Text = String.Concat(txtLog.Text, aux.ToString(), "\r\n");
            }
            else
            {
                MessageBox.Show("No devices detected!", "No devices!", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void btnDetectSerials_Click(object sender, EventArgs e)
        {
            string[] lstSerials = new string[50];

            //Clear the Results box
            txtLog.Clear();

            //Re-enumerate ports (in case anything has been plugged or unplugged)
            FeasaCom_EnumPorts();

            //Initialize string for responses (necessary to allocate space)
            // Has to be done just before receiving any response
            this.InitializeArrayOfStrings(ref lstSerials, 500);

            //Perform detection
            int nDetected = FeasaCom_DetectSN(lstSerials, "AUTO");
            if (nDetected > 0)
            {
                //List devices detected
                string aux = "Devices detected:\r\n";
                for (int i = 0; i < nDetected; i++)
                    aux += "...SN " + lstSerials[i] + "\r\n";
                txtLog.Text = String.Concat(txtLog.Text, aux.ToString(), "\r\n");
            }
            else
            {
                MessageBox.Show("No devices detected!", "No devices!", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        public void InitializeArrayOfStrings(ref string[] mArray, int StringSize)
        {
            //Function obtained from Feasa.cs - (c) Feasa 2019 All rights reserved
            for (int i = 0; i < mArray.Length; i++)
                mArray[i] = new string('\0', StringSize);
        }

        private void Label3_Click(object sender, EventArgs e)
        {

        }
    }
}
