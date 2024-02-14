/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: tools_lib
*
*  DESCRIPTION: This example demonstrates how to use the
*  library feasa_tools64.dll or its 32-bit equivalent
*  feasa_tools.dll, to extract or parse the strings returned
*  by the Feasa LED Analyser responses and convert them
*  into usable numerical values.
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

namespace tools_lib
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
        private static extern int FeasaCom_ListPortsDetected(int[] ListOfPortsDetected);

        [DllImport("feasa_tools64.dll")]
        private static extern int Feasa_Parse_RGBI(StringBuilder AnalyserResponse, ref byte Red, ref byte Green, ref byte Blue, ref int Intensity);

        [DllImport("feasa_tools64.dll")]
        private static extern int Feasa_Parse_HSI(StringBuilder AnalyserResponse, ref float Hue, ref int Saturation, ref int Intensity);


        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;
            int nPorts;
            int[] PortsList = new int[100];

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Retrieve the list of serial ports detected
            nPorts = FeasaCom_ListPortsDetected(PortsList);
            if (nPorts>0) {
                //List all serial prots detected
                for (i = 0; i < nPorts; i++)
                {
                    lstPorts.Items.Add(PortsList[i].ToString());
                }
                //Select the first port of the list
                lstPorts.SelectedIndex = 0;
            }
        }

        private void cmdRead_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            string aux;
            string numfib;
            StringBuilder buffer = new StringBuilder(100);
            float Hue=0;
            int Saturation=0, Intensity=0;
            byte Red=0, Green=0, Blue=0;

            //Clear the Results box
            lblHue.Text = "0.0";
            lblSat.Text = "0";
            lblIntensity.Text = "0";
            lblRed.Text = "0";
            lblGreen.Text = "0";
            lblBlue.Text = "0";

            //Set the port number
            int.TryParse(lstPorts.Items[lstPorts.SelectedIndex].ToString(), out portNumber);

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

                //Send command to the LED Analyser
                numfib = String.Concat("00", txtFiber.Text); //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                numfib = numfib.Substring(numfib.Length - 2);
                aux = String.Concat("GETHSI", numfib);
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, aux, buffer);
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

                //Parse values
                Feasa_Parse_HSI(buffer, ref Hue, ref Saturation, ref Intensity);

                //Send command to the LED Analyser
                aux = String.Concat("GETRGBI", numfib);
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, aux, buffer);
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

                //Parse values
                Feasa_Parse_RGBI(buffer, ref Red, ref Green, ref Blue, ref Intensity);

                //Shows the received data in the screen
                lblHue.Text = Hue.ToString(); //set hue
                lblSat.Text = Saturation.ToString(); //set saturation
                lblRed.Text = Red.ToString(); //set red
                lblGreen.Text = Green.ToString(); //set green
                lblBlue.Text = Blue.ToString(); //set blue
                lblIntensity.Text = Intensity.ToString(); //set intensity

                //Close the port
                FeasaCom_Close(portNumber);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

    }
}
