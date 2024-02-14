/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: getHSI
*
*  DESCRIPTION: This example demonstrates how to perform a
*  capture from the Feasa LED Analyser and then retrieve the
*  Hue, Saturation and Intensity values from a given
*  Fiber/sensor number, extracting the numerical values from
*  the string received.
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

namespace getHSI_simple
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

        private void cmdRead_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            string aux;
            StringBuilder buffer = new StringBuilder(100);

            //Clear the Results box
            lblHue.Text = "0.0";
            lblSat.Text = "0";
            lblIntensity.Text = "0";

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

                //Send command to the LED Analyser
                aux = String.Concat("00", txtFiber.Text); //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
                aux = aux.Substring(aux.Length - 2);
                aux = String.Concat("GETHSI", aux);
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

                //Shows the received data in the screen
                //(Split the response using the char Space as separator and returns the result into a string)
                aux = buffer.ToString();
                string[] auxlist = aux.Split(' ');
                lblHue.Text = auxlist[0]; //get hue
                lblSat.Text = auxlist[1]; //get saturation
                lblIntensity.Text = auxlist[2]; //get intensity

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
