/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: getRGBI
*
*  DESCRIPTION: This example demonstrates how to perform a
*  capture from the Feasa LED Analyser and retrieve the RGBI
*  color data for all the fibers, extracting each one of the
*  data values from the string received and displaying them
*  on a formatted table
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

namespace getRGBI
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
        private static extern int FeasaCom_IsPortAvailable(int CommPort);


        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            once your application is running */
            //FeasaCom_EnumPorts();

            //List available ports
            for (i = 1; i <= 100; i++)
            {
                if (FeasaCom_IsPortAvailable(i)==1) lstPorts.Items.Add(i.ToString());
            }
            //Select the first port of the list
            if (lstPorts.Items.Count>0) lstPorts.SelectedIndex = 0;

            //Fill the list with the number of fibers
            lstNumFibs.Items.Add("20");
            lstNumFibs.Items.Add("15");
            lstNumFibs.Items.Add("10");
            lstNumFibs.Items.Add("3");
            lstNumFibs.Items.Add("2");
            lstNumFibs.SelectedIndex = 0; //select 20 fibers as default

            //Setup data grid
            dataGrid.RowCount = 20;
            //Initialize grid values
            for (i = 0; i < 19; i++)
            {
                dataGrid.Rows[i].Cells[0].Value = 0;
                dataGrid.Rows[i].Cells[1].Value = 0;
                dataGrid.Rows[i].Cells[2].Value = 0;
                dataGrid.Rows[i].Cells[3].Value = 0;
            }
        }

        private void cmdCapture_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            StringBuilder buffer = new StringBuilder(100);

            //Set the port number
            portNumber = Convert.ToInt32(lstPorts.Items[lstPorts.SelectedIndex]);

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
                else
                {
                    //Shows info message
                    MessageBox.Show("Capture successful!");
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

        private void cmdRead_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            string aux;
            int Sensor;
            int NumFibers;
            StringBuilder buffer = new StringBuilder(100);

            //Set the port number
            portNumber = Convert.ToInt32(lstPorts.Items[lstPorts.SelectedIndex]);

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

                //Get the number of fibers
                NumFibers = int.Parse(lstNumFibs.Text);

                for (Sensor = 1; Sensor <= NumFibers; Sensor++)
                {
                    //Send command to the LED Analyser
                    resp = FeasaCom_Send(portNumber, "GETRGBI" + Sensor.ToString("00"), buffer);
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
                    //Note in the follofing lines that -1 is added because the index should start from 0
                    dataGrid.Rows[Sensor - 1].Cells[0].Value = int.Parse(auxlist[0]); //Red
                    dataGrid.Rows[Sensor - 1].Cells[1].Value = int.Parse(auxlist[1]); //Green
                    dataGrid.Rows[Sensor - 1].Cells[2].Value = int.Parse(auxlist[2]); //Blue
                    dataGrid.Rows[Sensor - 1].Cells[3].Value = int.Parse(auxlist[3]); //Intensity

                } //for

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
