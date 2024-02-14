/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: capture_modes
*
*  DESCRIPTION: This example demonstrates the different methods
*  available for performing measurements (captures) on the
*  LED Analyser. Then, the responses received are parsed and
*  the numerical values are exatracted and printed to a
*  grid-style output.
*  This example uses a helper function to format the received
*  decimal string to the default Local Decimal character.
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

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_SetResponseTimeout(uint Timeout);

        [DllImport("feasacom64.dll")]
        private static extern int FeasaCom_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);


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
            while the application is running */
            //FeasaCom_EnumPorts();

            //List available ports
            for (i = 1; i <= 100; i++)
            {
                if (FeasaCom_IsPortAvailable(i)==1) lstPorts.Items.Add(i.ToString());
            }
            //Select the first port of the list
            if (lstPorts.Items.Count > 0) lstPorts.SelectedIndex = 0;

            //Fill the list with the number of fibers
            lstNumFibs.Items.Add("20");
            lstNumFibs.Items.Add("15");
            lstNumFibs.Items.Add("10");
            lstNumFibs.Items.Add("3");
            lstNumFibs.Items.Add("2");
            lstNumFibs.SelectedIndex = 0; //select 20 fibers as default

            //Fill list with the Capture Mode
            lstCaptureMode.Items.Add("AUTO");
            lstCaptureMode.Items.Add("MANUAL");
            lstCaptureMode.Items.Add("PWM: AUTO-RANGE & AUTO-FRAMING");
            lstCaptureMode.Items.Add("PWM: MANUAL-RANGE & AUTO-FRAMING");
            lstCaptureMode.Items.Add("PWM: AUTO-RANGE & MANUAL-FRAMING");
            lstCaptureMode.Items.Add("PWM: MANUAL-RANGE & MANUAL-FRAMING");
            lstCaptureMode.SelectedIndex = 0;

            //Fill list with the Capture Ranges
            lstCaptureRange.Items.Add("LOW");
            lstCaptureRange.Items.Add("MEDIUM");
            lstCaptureRange.Items.Add("HIGH");
            lstCaptureRange.Items.Add("SUPER");
            lstCaptureRange.Items.Add("ULTRA");
            lstCaptureRange.SelectedIndex = 2;

            //Fill list with the frmes
            for (i = 1; i <= 15; i++)
            {
                lstCaptureFrame.Items.Add(i.ToString());
            }
            lstCaptureFrame.SelectedIndex = 4;

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
            string CaptureCommand = "";

            //Set the port number
            portNumber = Convert.ToInt32(lstPorts.Items[lstPorts.SelectedIndex]);

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Change maximum timeout to avoid timeout events due to long captures (manual/PWM)
            FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

            //Open port
            if (FeasaCom_Open(portNumber, "57600") == 1)
            {
                //No error

                //Compose capture command
                if (lstCaptureMode.SelectedIndex == 0)
                {
                    CaptureCommand = "CAPTURE";
                }
                else if (lstCaptureMode.SelectedIndex == 1)
                {
                    CaptureCommand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString();
                }
                else if (lstCaptureMode.SelectedIndex == 2)
                {
                    CaptureCommand = "CAPTUREPWM";
                }
                else if (lstCaptureMode.SelectedIndex == 3)
                {
                    CaptureCommand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString() + "PWM";
                }
                else if (lstCaptureMode.SelectedIndex == 4)
                {
                    CaptureCommand = "CAPTUREPWM" + (lstCaptureFrame.SelectedIndex + 1).ToString("00");
                }
                else if (lstCaptureMode.SelectedIndex == 5)
                {
                    CaptureCommand = "CAPTURE" + (lstCaptureRange.SelectedIndex + 1).ToString() + "PWM" + (lstCaptureFrame.SelectedIndex + 1).ToString("00");
                }

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Send(portNumber, CaptureCommand, buffer);
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
                    //format command (number to two digits string)
                    if (Sensor < 10)
                    {
                        aux = String.Concat("GETRGBI0", Sensor.ToString());
                    }
                    else
                    {
                        aux = String.Concat("GETRGBI", Sensor.ToString());
                    }
                    //Send command to the LED Analyser
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

        private void btnCaptureEasy_Click(object sender, EventArgs e)
        {
            int portNumber;
            int resp;
            StringBuilder buffer = new StringBuilder(100);
            int isPWM, CaptureRange, CapturePWMFrames;

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

                //Compose capture command
                isPWM = 0;
                CaptureRange = 0;
                CapturePWMFrames = 0;
                if (lstCaptureMode.SelectedIndex == 0)
                {
                    isPWM = 0;
                    CaptureRange = 0;
                }
                else if (lstCaptureMode.SelectedIndex == 1)
                {
                    isPWM = 0;
                    CaptureRange = lstCaptureRange.SelectedIndex + 1;
                }
                else if (lstCaptureMode.SelectedIndex == 2)
                {
                    isPWM = 1;
                    CaptureRange = 0;
                    CapturePWMFrames = 0;
                }
                else if (lstCaptureMode.SelectedIndex == 3)
                {
                    isPWM = 1;
                    CaptureRange = lstCaptureRange.SelectedIndex + 1;
                    CapturePWMFrames = 0;
                }
                else if (lstCaptureMode.SelectedIndex == 4)
                {
                    isPWM = 1;
                    CaptureRange = 0;
                    CapturePWMFrames = lstCaptureFrame.SelectedIndex + 1;
                }
                else if (lstCaptureMode.SelectedIndex == 5)
                {
                    isPWM = 1;
                    CaptureRange = lstCaptureRange.SelectedIndex + 1;
                    CapturePWMFrames = lstCaptureFrame.SelectedIndex + 1;
                }

                //Send command to the LED Analyser
                //You can nottice that there is no need to send the CR + LF characters (the command Send does it for you automatically)
                resp = FeasaCom_Capture(portNumber, isPWM, CaptureRange, CapturePWMFrames);
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
    }
}
