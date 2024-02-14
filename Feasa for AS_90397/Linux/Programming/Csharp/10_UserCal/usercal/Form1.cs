/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: UserCal
*
*  DESCRIPTION: This example demonstrates how to use the
*  UserCal library embedded in the Feasa SO Library in order
*  to ease the integration of the calibration process in any
*  user custom software.
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

namespace capture_and_read
{
    public partial class Form1 : Form
    {
        const string SOpath = "libfeasacom.x86_64.so";

        [DllImport(SOpath)]
        private static extern int FeasaCom_Open(string DevPath, int Baudrate);

        [DllImport(SOpath)]
        private static extern int FeasaCom_Close(string DevPath);

        [DllImport(SOpath)]
        private static extern int FeasaCom_Send(string DevPath, string Command, StringBuilder ResponseText);

        [DllImport(SOpath)]
        private static extern int FeasaCom_IsPortAvailable(string DevPath);

        [DllImport(SOpath)]
        private static extern int FeasaCom_Capture(string DevPath, int isPWM, int CaptureRange, int CapturePWMFrames);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_ResetIntensity(string DevPath, int Fiber, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_GetIntensityGain(string DevPath, int Fiber, ref int Gain);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_SetIntensityGain(string DevPath, int Fiber, int Gain, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_AdjustIntensity(string DevPath, int Fiber, int IntensityRef, int isPWM, int CaptureRange, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_ResetxyOffsets(string DevPath, int Fiber, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_GetxyOffsets(string DevPath, int Fiber, ref float xOffset, ref float yOffset);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_SetxyOffsets(string DevPath, int Fiber, float xOffset, float yOffset, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_AdjustxyOffsets(string DevPath, int Fiber, float xRef, float yRef, int ToFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_ResetWavelengthOffset(string DevPath, int Fiber, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_GetWavelengthOffset(string DevPath, int Fiber, ref int WavelengthOffset);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_SetWavelengthOffset(string DevPath, int Fiber, int WavelengthOffset, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_AdjustWavelengthOffset(string DevPath, int Fiber, int WavelengthRef, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_ResetAbsInt(string DevPath, int Fiber, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_GetAbsIntFactor(string DevPath, int Fiber, ref double Factor);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_SetAbsIntFactor(string DevPath, int Fiber, double Factor, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_AdjustAbsInt(string DevPath, int Fiber, double AbsIntRef, int toFlash);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_ResetRGBAdj(string DevPath, int Fiber);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_TakeRGBCurrentValues(string DevPath, int Fiber, char Color);

        [DllImport(SOpath)]
        private static extern int FeasaCom_UserCal_AdjustRGB(string DevPath, int Fiber, float xRefRed, float yRefRed, double AbsIntRefRed, float xRefGreen, float yRefGreen, double AbsIntRefGreen, float xRefBlue, float yRefBlue, double AbsIntRefBlue);

        [DllImport(SOpath)]
        private static extern int FeasaCom_GetError_Description(StringBuilder ErrorDescription);

        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            int i;

            //Add the port numbers in the list from 0 to 16
            for (i = 0; i <= 50; i++)
            {
                if (FeasaCom_IsPortAvailable("/dev/ttyS" + i.ToString()) == 1)
                    lstPorts.Items.Add("/dev/ttyS" + i.ToString());
                if (FeasaCom_IsPortAvailable("/dev/ttyUSB" + i.ToString()) == 1)
                    lstPorts.Items.Add("/dev/ttyUSB" + i.ToString());
            }
            //Select the first port of the list
            lstPorts.SelectedIndex = 0;

            //Select Capture type
            lstCapture.SelectedIndex = 0;

            //Select target
            optRAM.Checked = true;
        }

        private void btnBalanceInt_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int toFlash = optFlash.Checked ? 1 : 0;
            int NFIBERS = (int)numFibers.Value;
            int isPWM = chkPWM.Checked ? 1 : 0;
            int CaptureRange = lstCapture.SelectedIndex;
            const int PWMframes = 5;

            if (lstPorts.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            /*This command enumerates the existing ports to find out
            what are the serial ports existing on your computer and
            the devices connected to them. You need to execute this
            command everytime you plug or unplug a Feasa Device,
            while the application is running */
            //FeasaCom_EnumPorts();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //-------------------------------------------------
                // RELATIVE INTENSITY ADJUSTMENT
                //-------------------------------------------------

                //Reset intensities
                for (i = 1; i <= NFIBERS; i++)
                    FeasaCom_UserCal_ResetIntensity(DevPath, i, toFlash);

                //Capture
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

                //Read back intensities / Calculates average intensity
                int AvgInt = 0;
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_Send(DevPath, "GETINTENSITY" + i.ToString("00"), buffer);
                    if (resp == -1)
                    {
                        MessageBox.Show("Error: unable to send the command!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                    else if (resp == 0)
                    {
                        MessageBox.Show("Timeout detected!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                    int auxint = 0;
                    int.TryParse(buffer.ToString(), out auxint);
                    AvgInt += auxint;
                }
                AvgInt = AvgInt / NFIBERS;
                txtLog.Text = String.Concat(txtLog.Text, "AvgInt=" + AvgInt.ToString(), "\r\n");

                //Adjustment
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_UserCal_AdjustIntensity(DevPath, i, AvgInt, isPWM, CaptureRange, toFlash);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Check results
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
                resp = FeasaCom_Send(DevPath, "GETINTENSITYALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, "Results:", "\r\n", buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void btnAdjustAbsInt_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int toFlash = optFlash.Checked ? 1 : 0;
            int NFIBERS = (int)numFibers.Value;
            int isPWM = chkPWM.Checked ? 1 : 0;
            int CaptureRange = lstCapture.SelectedIndex;
            const int PWMframes = 5;

            if (lstPorts.SelectedIndex == -1)
                return;

            if (lstCapture.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //-------------------------------------------------
                // ABSOLUTE INTENSITY ADJUSTMENT
                //-------------------------------------------------

                //Reset factors
                for (i = 1; i <= NFIBERS; i++)
                    FeasaCom_UserCal_ResetAbsInt(DevPath, i, toFlash);

                //Capture
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

                //Read reference
                double AbsIntRef = 2.355E-02;
                double.TryParse(FormatDecimal(txtRefAbsInt.Text), out AbsIntRef);
                txtLog.Text = String.Concat(txtLog.Text, "AbsIntRef=" + AbsIntRef.ToString(), "\r\n");

                //Calibration
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_UserCal_AdjustAbsInt(DevPath, i, AbsIntRef, toFlash);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Check results
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
                resp = FeasaCom_Send(DevPath, "GETABSINTALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

        }

        private void btnAdjustWavelength_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int toFlash = optFlash.Checked ? 1 : 0;
            int NFIBERS = (int)numFibers.Value;
            int isPWM = chkPWM.Checked ? 1 : 0;
            int CaptureRange = lstCapture.SelectedIndex;
            const int PWMframes = 5;

            if (lstPorts.SelectedIndex == -1)
                return;

            if (lstCapture.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //-------------------------------------------------
                // WAVELENGTH OFFSETS ADJUSTMENT
                //-------------------------------------------------

                //Reset offsets
                for (i = 1; i <= NFIBERS; i++)
                    FeasaCom_UserCal_ResetWavelengthOffset(DevPath, i, toFlash);

                //Capture
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

                int Wref = 0;
                int.TryParse(txtRefWavelength.Text, out Wref);
                txtLog.Text = String.Concat(txtLog.Text, "Wref=" + Wref.ToString(), "\r\n");

                //Calibration
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_UserCal_AdjustWavelengthOffset(DevPath, i, Wref, toFlash);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Check results
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
                resp = FeasaCom_Send(DevPath, "GETwavelengthALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

        }

        private void btnAdjustxy_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int toFlash = optFlash.Checked ? 1 : 0;
            int NFIBERS = (int)numFibers.Value;
            int isPWM = chkPWM.Checked ? 1 : 0;
            int CaptureRange = lstCapture.SelectedIndex;
            const int PWMframes = 5;

            if (lstPorts.SelectedIndex == -1)
                return;

            if (lstCapture.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //-------------------------------------------------
                // xy OFFSETS ADJUSTMENT
                //-------------------------------------------------

                //Reset offsets
                for (i = 1; i <= NFIBERS; i++)
                    FeasaCom_UserCal_ResetxyOffsets(DevPath, i, toFlash);

                //Capture
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

                //Read back results
                float xRef = 0, yRef = 0;
                float.TryParse(FormatDecimal(txtRefx.Text), out xRef);
                float.TryParse(FormatDecimal(txtRefy.Text), out yRef);
                txtLog.Text = String.Concat(txtLog.Text, "xRef=" + xRef.ToString() + ", yRef=" + yRef.ToString(), "\r\n");

                //Calibration
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_UserCal_AdjustxyOffsets(DevPath, i, xRef, yRef, toFlash);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Check results
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
                resp = FeasaCom_Send(DevPath, "GETxyALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }

        }

        private void btnAdjustRGB_Click(object sender, EventArgs e)
        {
            string DevPath;
            int resp;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int toFlash = optFlash.Checked ? 1 : 0;
            int NFIBERS = (int)numFibers.Value;
            int isPWM = chkPWM.Checked ? 1 : 0;
            int CaptureRange = lstCapture.SelectedIndex;
            const int PWMframes = 5;

            if (lstPorts.SelectedIndex == -1)
                return;

            if (lstCapture.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //-------------------------------------------------
                // RGB ADJUSTMENT
                //-------------------------------------------------

                //Reset
                for (i = 1; i <= NFIBERS; i++)
                    FeasaCom_UserCal_ResetRGBAdj(DevPath, i);

                //Read back reference values
                float xRefR = 0, yRefR = 0; double AbsIntRefR = 0;
                float xRefG = 0, yRefG = 0; double AbsIntRefG = 0;
                float xRefB = 0, yRefB = 0; double AbsIntRefB = 0;
                float.TryParse(FormatDecimal(txtRefxR.Text), out xRefR);
                float.TryParse(FormatDecimal(txtRefyR.Text), out yRefR);
                double.TryParse(FormatDecimal(txtRefAbsIntR.Text), out AbsIntRefR);
                float.TryParse(FormatDecimal(txtRefxG.Text), out xRefG);
                float.TryParse(FormatDecimal(txtRefyG.Text), out yRefG);
                double.TryParse(FormatDecimal(txtRefAbsIntG.Text), out AbsIntRefG);
                float.TryParse(FormatDecimal(txtRefxB.Text), out xRefB);
                float.TryParse(FormatDecimal(txtRefyB.Text), out yRefB);
                double.TryParse(FormatDecimal(txtRefAbsIntB.Text), out AbsIntRefB);
                txtLog.Text = String.Concat(txtLog.Text, "RED: xRef=" + xRefR.ToString() + ", yRef=" + yRefR.ToString() + ", AbsIntRef=" + AbsIntRefR.ToString(), "\r\n");
                txtLog.Text = String.Concat(txtLog.Text, "GREEN: xRef=" + xRefG.ToString() + ", yRef=" + yRefG.ToString() + ", AbsIntRef=" + AbsIntRefG.ToString(), "\r\n");
                txtLog.Text = String.Concat(txtLog.Text, "BLUE: xRef=" + xRefB.ToString() + ", yRef=" + yRefB.ToString() + ", AbsIntRef=" + AbsIntRefB.ToString(), "\r\n");

                //Pre-cal
                string[] COLORS = { "RED", "GREEN", "BLUE" };
                for (int c = 0; c < 3; c++)
                {
                    if (MessageBox.Show("Please, switch on " + COLORS[c] + " LED and click OK to continue", "Color measurement", MessageBoxButtons.OKCancel, MessageBoxIcon.Information) == DialogResult.Cancel)
                        return;

                    //Capture
                    resp = FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }

                    for (i = 1; i <= NFIBERS; i++)
                    {
                        //Store actual measurements
                        resp = FeasaCom_UserCal_TakeRGBCurrentValues(DevPath, i, COLORS[c][0]);
                        if (resp != 1)
                        {
                            FeasaCom_GetError_Description(buffer);
                            MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            FeasaCom_Close(DevPath);
                            return;
                        }
                    } //FOR_FIBERS
                } //FOR_COLORS

                //Calibration
                for (i = 1; i <= NFIBERS; i++)
                {
                    resp = FeasaCom_UserCal_AdjustRGB(DevPath, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB);
                    if (resp != 1)
                    {
                        FeasaCom_GetError_Description(buffer);
                        MessageBox.Show("Error:" + buffer.ToString(), "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        FeasaCom_Close(DevPath);
                        return;
                    }
                }

                //Check results
                FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
                resp = FeasaCom_Send(DevPath, "GETxyALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");
                resp = FeasaCom_Send(DevPath, "GETABSINTALL", buffer);
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
                txtLog.Text = String.Concat(txtLog.Text, buffer.ToString(), "\r\n");

                //Close the port
                FeasaCom_Close(DevPath);
            }
            else
            {
                //Error: unable to open the selected port
                MessageBox.Show("Unable to open the port", "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
        }

        private void btnReadParams_Click(object sender, EventArgs e)
        {
            string DevPath;
            StringBuilder buffer = new StringBuilder(1024);
            int i;
            int NFIBERS = (int)numFibers.Value;
            int Gain = 0;
            float xOffset = 0, yOffset = 0;
            int WavelengthOffset = 0;
            double AbsIntFactor = 0;

            if (lstPorts.SelectedIndex == -1)
                return;

            //Set the port number
            DevPath = lstPorts.Items[lstPorts.SelectedIndex].ToString();

            //Clear the Results box
            txtLog.Clear();

            //Open port
            if (FeasaCom_Open(DevPath, 57600) == 1)
            {
                //No error

                //Retrieve Intensity gains
                for (i = 1; i <= NFIBERS; i++)
                {
                    FeasaCom_UserCal_GetIntensityGain(DevPath, i, ref Gain);
                    txtLog.Text = String.Concat(txtLog.Text, "Int Gain " + i.ToString("00") + ": " + Gain.ToString(), "\r\n");
                }

                //Retrieve xy Offsets
                for (i = 1; i <= NFIBERS; i++)
                {
                    FeasaCom_UserCal_GetxyOffsets(DevPath, i, ref xOffset, ref yOffset);
                    txtLog.Text = String.Concat(txtLog.Text, "xy Offsets " + i.ToString("00") + ": " + xOffset.ToString() + "; " + yOffset.ToString(), "\r\n");
                }

                //Retrieve Wavelength Offsets
                for (i = 1; i <= NFIBERS; i++)
                {
                    FeasaCom_UserCal_GetWavelengthOffset(DevPath, i, ref WavelengthOffset);
                    txtLog.Text = String.Concat(txtLog.Text, "Wl Offsets " + i.ToString("00") + ": " + WavelengthOffset.ToString(), "\r\n");
                }

                //Retrieve Abs Int Factor
                for (i = 1; i <= NFIBERS; i++)
                {
                    FeasaCom_UserCal_GetAbsIntFactor(DevPath, i, ref AbsIntFactor);
                    txtLog.Text = String.Concat(txtLog.Text, "Abs Int Factor " + i.ToString("00") + ": " + AbsIntFactor.ToString(), "\r\n");
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

        public static string FormatDecimal(string Number)
        {
            bool DecimalFormatDot = false;

            //Set the decimal format
            float auxfloat = 0;
            if (float.TryParse("3.21", out auxfloat))
            {
                if (auxfloat == 3.21f)
                    DecimalFormatDot = true;
                else
                    DecimalFormatDot = false;
            }
            else
            {
                DecimalFormatDot = false;
            }

            if (DecimalFormatDot)
                return Number.Replace(',', '.');
            else
                return Number.Replace('.', ',');
        }
    }
}
