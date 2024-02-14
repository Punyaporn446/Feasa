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
*  a method to load the SO library and to call the functions
*  provided.
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
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Runtime.InteropServices;

namespace WindowsFormsApplication1
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

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            ///Add the port numbers to the list
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

        private void cmdRead_Click(object sender, EventArgs e)
        {
            string DevPath;
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
            if ( FeasaCom_Open(DevPath, 57600)==1 )
            {
                //No error

                //Send command to the Led Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                FeasaCom_Send(DevPath, "GETSERIAL", buffer);

                //Shows the received data in the screen
                txtLog.Text = String.Concat("Serial:",buffer.ToString());

                //Close the port
                FeasaCom_Close(DevPath);
            } else {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port");
            }

        }
    }
}
