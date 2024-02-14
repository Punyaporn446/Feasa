/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: UserCal
*
*  DESCRIPTION: This example demonstrates how to use the
*  UserCal library embedded in the Feasa DLL in order to ease
*  the integration of the calibration process in any user
*  custom software.
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

//==============================================================================
// Include files

#include <ansi_c.h>
#include <cvirte.h>     
#include <userint.h>
#include <windows.h>
#include "radioGroup.h"
#include "usercal.h"
#include "toolbox.h"
#include "feasacom.h"

// Static global variables

static int panelHandle;

int BalanceIntToAvg(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash);
int AdjustAbsInt(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash);
int AdjustWavelengthOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash);
int AdjustxyOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash);
int AdjustRGB(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash);
int ReadParams(int DevicePort, int nFibers);

//-------------- MAIN ---------------------------------------------------------
int main (int argc, char *argv[])
{
    int error = 0;
	int i;
	char aux[15];
	int nPorts;
	int PortsList[100];

    /* initialize and load resources */
    nullChk (InitCVIRTE (0, argv, 0));
    errChk (panelHandle = LoadPanel (0, "usercal.uir", PANEL));
	Radio_ConvertFromTree (panelHandle, PANEL_OPTRAMFLASH);
	
	//Retrieve the list of ports detected
	nPorts = FeasaCom_ListPortsDetected(PortsList);
	if (nPorts>0) {
		//Add items to the ports list
		for (i=0; i<nPorts; i++) {
			sprintf(aux, "COM %d", PortsList[i]);
			InsertListItem(panelHandle, PANEL_LSTCOMMPORT, i, aux, PortsList[i]);
		}
		SetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, 0);
	}
	
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 0, "AUTO", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 1, "LOW", 1);
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 2, "MEDIUM", 2);
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 3, "HIGH", 3);
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 4, "SUPER", 4);
	InsertListItem(panelHandle, PANEL_LSTCAPTURE, 5, "ULTRA", 5);
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTURE, 0);
	
    /* display the panel and run the user interface */
    errChk (DisplayPanel (panelHandle));
    errChk (RunUserInterface ());

Error:
    /* clean up */
    DiscardPanel (panelHandle);
    return 0;
}

//==============================================================================
// UI callback function prototypes

/// HIFN Exit when the user dismisses the panel.
int CVICALLBACK panelCB (int panel, int event, void *callbackData,
        int eventData1, int eventData2)
{
    if (event == EVENT_CLOSE) {
        QuitUserInterface (0);
	}
    return 0;
}

int CVICALLBACK btnAdjust (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int r;
	int port; //holds the port to be opened
	int nFibers;
	int CaptureRange;
	int isPWM;
	int toFlash;
	const int PWMframes = 0; //Auto-framing

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			
			//Get selected port
			port = -1;
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &r);
			if (r<0) {
				MessagePopup("Information", "Please, select a port first.");
				return 0;
			}
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
			
			//Get Number of fibers
			GetCtrlVal(panelHandle, PANEL_NUMFIBERS, &nFibers);
			
			//Get Capture range
			r = -1;
			GetCtrlIndex(panelHandle, PANEL_LSTCAPTURE, &r);
			if (r<0) {
				MessagePopup("Information", "Please, select the capture range.");
				return 0;
			}
			GetCtrlVal(panelHandle, PANEL_LSTCAPTURE, &CaptureRange);
			
			//Find out if PWM is active
			GetCtrlVal(panelHandle, PANEL_CHKPWM, &r);
			isPWM = (r==TRUE)? 1 : 0;
			
			//Get storage destination
			Radio_GetMarkedOption(panelHandle, PANEL_OPTRAMFLASH, &toFlash);
			
			//Clear Log
			ResetTextBox(panelHandle, PANEL_TXTLOG, "");
			
			//Open the port previously selected, where it's supposed the LED Analyser to be.
			if ( FeasaCom_Open(port, "57600")==0 ) {
				MessagePopup("Error", "Unable to open port!");
				return 0;
			}
			
			switch (control) {
				case PANEL_BTNBALANCEINT:
					BalanceIntToAvg(port, nFibers, isPWM, CaptureRange, PWMframes, toFlash);
					break;
				case PANEL_BTNADJUSTABSINT:
					AdjustAbsInt(port, nFibers, isPWM, CaptureRange, PWMframes, toFlash);
					break;
				case PANEL_BTNADJUSTWAVELENGTH:
					AdjustWavelengthOffsets(port, nFibers, isPWM, CaptureRange, PWMframes, toFlash);
					break;
				case PANEL_BTNADJUSTXY:
					AdjustxyOffsets(port, nFibers, isPWM, CaptureRange, PWMframes, toFlash);
					break;
				case PANEL_BTNADJUSTRGB:
					AdjustRGB(port, nFibers, isPWM, CaptureRange, PWMframes, toFlash);
					break;
				case PANEL_BTNREADPARAMS:
					ReadParams(port, nFibers);
					break;
			}
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			break;
	}
	return 0;
}

int BalanceIntToAvg(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash)
{
	int resp;
	char buffer[1024]; // Used to store the response from the LED Analyser
	int i;
	int auxint = 0;
	char command[100];

	//-------------------------------------------------
	// RELATIVE INTENSITY ADJUSTMENT
	//-------------------------------------------------

	//Reset intensities
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetIntensity(DevicePort, i, toFlash);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Read back intensities / Calculates average intensity
	int AvgInt = 0;
	for (i = 1; i <= nFibers; i++)
	{
		sprintf(command, "GETINTENSITY%02d", i);
		resp = FeasaCom_Send(DevicePort, command, buffer);
		if (resp == -1)
		{
			MessagePopup("Error", "Unable to send the command!");
			return 0;
		}
		else if (resp == 0)
		{
			MessagePopup("Error", "Timeout detected!");
			return 0;
		}
		sscanf(buffer, "%d", &auxint);
		AvgInt += auxint;
	}
	AvgInt = AvgInt / nFibers;
	sprintf(buffer, "AvgInt=%d\n", AvgInt);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	//Adjustment
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustIntensity(DevicePort, i, AvgInt, isPWM, CaptureRange, toFlash);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			MessagePopup("Error", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETINTENSITYALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "Unable to send the command!");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, "Results:\n");
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	return 1;
}

int AdjustAbsInt(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash)
{
	int resp;
	char buffer[1024];
	int i;

	//Read reference
	double AbsIntRef = 2.355E-02;
	GetCtrlVal(panelHandle, PANEL_TXTREFABSINT, &AbsIntRef);
	sprintf(buffer, "AbsIntRef=%0.5E\n", AbsIntRef);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	//-------------------------------------------------
	// ABSOLUTE INTENSITY ADJUSTMENT
	//-------------------------------------------------

	//Reset factors
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetAbsInt(DevicePort, i, toFlash);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustAbsInt(DevicePort, i, AbsIntRef, toFlash);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			MessagePopup("Error", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETABSINTALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, "Results:\n");
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	return 1;
}

int AdjustWavelengthOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash)
{
	int resp;
	char buffer[1024];
	int i;

	//Read reference
	int Wref = 0;
	GetCtrlVal(panelHandle, PANEL_TXTREFWAVELENGTH, &Wref);
	sprintf(buffer, "Wref=%dnm\n", Wref);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	//-------------------------------------------------
	// WAVELENGTH OFFSETS ADJUSTMENT
	//-------------------------------------------------

	//Reset offsets
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetWavelengthOffset(DevicePort, i, toFlash);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustWavelengthOffset(DevicePort, i, Wref, toFlash);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			MessagePopup("Error", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETwavelengthALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, "Results:\n");
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	return 1;
}

int AdjustxyOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash)
{
	int resp;
	char buffer[1024];
	int i;

	float xRef = 0, yRef = 0;
	GetCtrlVal(panelHandle, PANEL_TXTREFX, &xRef);
	GetCtrlVal(panelHandle, PANEL_TXTREFY, &yRef);
	sprintf(buffer, "xRef=%0.4f\n", xRef);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "yRef=%0.4f\n", yRef);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	//-------------------------------------------------
	// xy OFFSETS ADJUSTMENT
	//-------------------------------------------------

	//Reset offsets
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetxyOffsets(DevicePort, i, toFlash);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustxyOffsets(DevicePort, i, xRef, yRef, toFlash);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			MessagePopup("Error", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETxyALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, "Results:\n");
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	return 1;
}

int AdjustRGB(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes, int toFlash)
{
	int resp;
	char buffer[1024];
	int i, c;
	const char * COLORS[] = { "RED", "GREEN", "BLUE" };

	float xRefR = 0, yRefR = 0, xRefG = 0, yRefG = 0, xRefB = 0, yRefB = 0;
	double AbsIntRefR = 0, AbsIntRefG = 0, AbsIntRefB = 0;
	GetCtrlVal(panelHandle, PANEL_TXTREFXR, &xRefR);
	GetCtrlVal(panelHandle, PANEL_TXTREFYR, &yRefR);
	GetCtrlVal(panelHandle, PANEL_TXTREFABSINTR, &AbsIntRefR);
	GetCtrlVal(panelHandle, PANEL_TXTREFXG, &xRefG);
	GetCtrlVal(panelHandle, PANEL_TXTREFYG, &yRefG);
	GetCtrlVal(panelHandle, PANEL_TXTREFABSINTG, &AbsIntRefG);
	GetCtrlVal(panelHandle, PANEL_TXTREFXB, &xRefB);
	GetCtrlVal(panelHandle, PANEL_TXTREFYB, &yRefB);
	GetCtrlVal(panelHandle, PANEL_TXTREFABSINTB, &AbsIntRefB);
	sprintf(buffer, "xRef (RED)=%0.4f\n", xRefR);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "yRef (RED)=%0.4f\n", yRefR);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "AbsIntRef (RED)=%lf\n", AbsIntRefR);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "xRef (GREEN)=%0.4f\n", xRefG);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "yRef (GREEN)=%0.4f\n", yRefG);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "AbsIntRef (GREEN)=%lf\n", AbsIntRefG);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "xRef (BLUE)=%0.4f\n", xRefB);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "yRef (BLUE)=%0.4f\n", yRefB);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	sprintf(buffer, "AbsIntRef (BLUE)=%lf\n", AbsIntRefB);
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	//-------------------------------------------------
	// RGB ADJUSTMENT
	//-------------------------------------------------

	//Reset
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetRGBAdj(DevicePort, i);

	for (c = 0; c < 3; c++) {
		sprintf(buffer, "Please, switch on %s LED and press a key to continue...\n", COLORS[c]);
		MessagePopup("Switch on LED", buffer);

		//Capture
		FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

		//Store current measurements
		for (i = 1; i <= nFibers; i++)
		{
			resp = FeasaCom_UserCal_TakeRGBCurrentValues(DevicePort, i, (*COLORS[c]));
			if (resp != 1)
			{
				FeasaCom_GetError_Description(buffer);
				MessagePopup("Error", buffer);
				return 0;
			}
		}
	}

	//Adjustment
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustRGB(DevicePort, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			MessagePopup("Error", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETxyALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, "Results:\n");
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	resp = FeasaCom_Send(DevicePort, "GETABSINTALL", buffer);
	if (resp == -1)
	{
		MessagePopup("Error", "unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		MessagePopup("Error", "Timeout detected!\n");
		return 0;
	}
	SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);

	return 1;
}

int ReadParams(int DevicePort, int nFibers)
{
	char buffer[1024];
	int i;
	int Gain = 0;
	float xOffset = 0, yOffset = 0;
	int WavelengthOffset = 0;
	double AbsIntFactor = 0;

	//Retrieve Intensity gains
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetIntensityGain(DevicePort, i, &Gain);
		sprintf(buffer, "Int Gain %d: %d\n", i, Gain);
		SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	}

	//Retrieve xy Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetxyOffsets(DevicePort, i, &xOffset, &yOffset);
		sprintf(buffer, "xy Offsets %d: %0.4f; %0.4f\n", i, xOffset, yOffset);
		SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	}

	//Retrieve Wavelength Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetWavelengthOffset(DevicePort, i, &WavelengthOffset);
		sprintf(buffer, "Wl Offsets %d: %d\n", i, WavelengthOffset);
		SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	}

	//Retrieve Abs Int Factor
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetAbsIntFactor(DevicePort, i, &AbsIntFactor);
		sprintf(buffer, "Abs Int Factor %d: %E\n", i, AbsIntFactor);
		SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
	}

	return 1;
}
