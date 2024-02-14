/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: getserial
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser. It also shows
*  a method to load the DLL library and to call the functions
*  provided.
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

namespace WindowsFormsApplication1
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
            if ( FeasaCom_Open(portNumber, "57600")==1 )
            {
                //No error

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                FeasaCom_Send(portNumber, "GETSERIAL", buffer);

                //Shows the received data in the screen
                txtLog.Text = String.Concat("Serial:",buffer.ToString());

                //Close the port
                FeasaCom_Close(portNumber);
            } else {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port");
            }

        }
    }
}
