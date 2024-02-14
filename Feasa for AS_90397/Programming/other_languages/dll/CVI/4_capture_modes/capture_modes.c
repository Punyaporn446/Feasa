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

//==============================================================================
// Include files

#include <ansi_c.h>
#include <cvirte.h>     
#include <userint.h>
#include <windows.h>
#include "capture_modes.h"
#include "toolbox.h"
#include "feasacom.h"

// Static global variables

static int panelHandle;

//-------------- MAIN ---------------------------------------------------------
int main (int argc, char *argv[])
{
    int error = 0;
	int i, j;
	char aux[50];
	
    /* initialize and load resources */
    nullChk (InitCVIRTE (0, argv, 0));
    errChk (panelHandle = LoadPanel (0, "capture_modes.uir", PANEL));
	
	//Add items to the ports list
	j=0;
	for (i=1; i<250; i++) {
		if ( FeasaCom_IsPortAvailable(i)==1 ) {
			sprintf(aux, "COM %d", i);
			InsertListItem(panelHandle, PANEL_LSTCOMMPORT, j++, aux, i);
		}
	}
	GetNumListItems(panelHandle, PANEL_LSTCOMMPORT, &i);
	if ( i>0 ) SetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, 0);
	
	//Fill list with Capture ranges
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 0, "AUTO", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 1, "MANUAL", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 2, "PWM: AUTO-RANGE & AUTO-FRAMING", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 3, "PWM: MANUAL-RANGE & AUTO-FRAMING", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 4, "PWM: AUTO-RANGE & MANUAL-FRAMING", 0);
	InsertListItem(panelHandle, PANEL_LSTCAPTUREMODE, 5, "PWM: MANUAL-RANGE & MANUAL-FRAMING", 0);
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTUREMODE, 0);
	
	//Fill list with cature ranges
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 0, "LOW", 1);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 1, "MEDIUM", 2);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 2, "HIGH", 3);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 3, "SUPER", 4);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 4, "ULTRA", 5);
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTURERANGE, 2);
	
	//Fill list with frames
	for (i=0; i<15; i++) {
		sprintf(aux, "%d", i+1);
		InsertListItem(panelHandle, PANEL_LSTCAPTUREFRAME, i, aux, i+1);
	}														   
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTUREFRAME, 4);
	
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

int CVICALLBACK btnCaptureAndRead (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	
	char buffer[1024]; //Input Buffer used to communicate with the LED Analyser
					   //Ensure that it has enought size to fit all the data sent by the longest command used
	int port; //holds the port to be opened
	int auxint; //Auxiliar integer
	int r; //holds the response from the DLL commands
	int i, j;
	char command[100]; //Auxiliar string used to compose commands
	float Hue;
	int Saturation;
	long Intensity;

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			//Get selected port
			port = -1;
			//GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &port);
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
			if (port<0) {
				MessagePopup("Information", "Please, select a port first.");
				return 0;
			}
		
			//This command enumerates the existing ports to find out what are the serial
			// ports available on your computer and the devices connected to them.
			// This function needs to be executed any time that a Feasa device is
			// connected or disconnected, while the application is running
			//FeasaCom_EnumPorts();
			
			//Increase maximum timeout in order to avoid timeout events caused by long captures (manual, PWM...)
			FeasaCom_SetResponseTimeout(8000); //8000 milliseconds
			
			//Open the port previously selected, where it's supposed the LED Analyser to be.
			if ( FeasaCom_Open(port, "57600")==0 ) {
				MessagePopup("Error", "Unable to open port!");
				return 0;
			}
			
			//Compose Capture command
			GetCtrlIndex(panelHandle, PANEL_LSTCAPTUREMODE, &i);
			if (i==0) {
				strcpy(command, "CAPTURE");
			} else if (i==1) {
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &i);
				sprintf(command, "CAPTURE%d", i);
			} else if (i==2) {
				strcpy(command, "CAPTUREPWM");
			} else if (i==3) {
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &i);
				sprintf(command, "CAPTURE%dPWM", i);
			} else if (i==4) {
				GetCtrlVal(panelHandle, PANEL_LSTCAPTUREFRAME, &i);
				sprintf(command, "CAPTUREPWM%02d", i);
			} else if (i==5) {
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &i);
				GetCtrlVal(panelHandle, PANEL_LSTCAPTUREFRAME, &j);
				sprintf(command, "CAPTURE%dPWM%02d", i, j);
			}
			
			//Execute a capture in the Feasa LED Analyser
			r = FeasaCom_Send(port, command, &buffer[0]);
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!\nPossible Syntax Error detected.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Read back Color and Intensity measurements
			GetCtrlVal(panelHandle, PANEL_NUMFIBER, &auxint);
			sprintf(command, "GETHSI%.2d", auxint); //compose command
			r = FeasaCom_Send(port, command, &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Parse and format the response
			sscanf(buffer, "%f %d %d", &Hue, &Saturation, &Intensity);
			SetCtrlVal(panelHandle, PANEL_HUE, Hue);
			SetCtrlVal(panelHandle, PANEL_SAT, Saturation);
			SetCtrlVal(panelHandle, PANEL_INT, Intensity);
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			break;
	}
	return 0;
}

int CVICALLBACK btnCaptureAndReadEasy (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	
	char buffer[1024]; //Input Buffer used to communicate with the LED Analyser
					   //Ensure that it has enought size to fit all the data sent by the longest command used
	int port; //holds the port to be opened
	int auxint; //Auxiliar integer
	int r; //holds the response from the DLL commands
	int i;
	char command[20];
	int isPWM, CaptureRange, CapturePWMFrames;
	float Hue;
	int Saturation;
	long Intensity;

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			//Get selected port
			port = -1;
			//GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &port);
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
			if (port<0) {
				MessagePopup("Information", "Please, select a port first.");
				return 0;
			}
		
			//This command enumerates the existing ports to find out what are the serial
			// ports available on your computer and the devices connected to them.
			// This function needs to be executed any time that a Feasa device is
			// connected or disconnected, while the application is running
			//FeasaCom_EnumPorts();
			
			//Open the port previously selected, where it's supposed the LED Analyser to be.
			if ( FeasaCom_Open(port, "57600")==0 ) {
				MessagePopup("Error", "Unable to open port!");
				return 0;
			}
			
			//Initialize variables
			isPWM = 0;
			CaptureRange = 0;
			CapturePWMFrames = 0;
			
			//Compose Capture command
			GetCtrlIndex(panelHandle, PANEL_LSTCAPTUREMODE, &i);
			if (i==0) {
				isPWM = 0;
				CaptureRange = 0;
			} else if (i==1) {
				isPWM = 0;
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &CaptureRange);
			} else if (i==2) {
				isPWM = 1;
				CaptureRange = 0;
				CapturePWMFrames = 0;
			} else if (i==3) {
				isPWM = 1;
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &CaptureRange);
				CapturePWMFrames = 0;
			} else if (i==4) {
				isPWM = 1;
				CaptureRange = 0;
				GetCtrlVal(panelHandle, PANEL_LSTCAPTUREFRAME, &CapturePWMFrames);
			} else if (i==5) {
				isPWM = 1;
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &CaptureRange);
				GetCtrlVal(panelHandle, PANEL_LSTCAPTUREFRAME, &CapturePWMFrames);
			}
			
			//Execute a capture in the Feasa LED Analyser
			r = FeasaCom_Capture(port, isPWM, CaptureRange, CapturePWMFrames);
			if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Read back Color and Intensity measurements
			GetCtrlVal(panelHandle, PANEL_NUMFIBER, &auxint);
			sprintf(command, "GETHSI%.2d", auxint); //compose command
			r = FeasaCom_Send(port, command, &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Parse and format the response
			sscanf(buffer, "%f %d %d", &Hue, &Saturation, &Intensity);
			SetCtrlVal(panelHandle, PANEL_HUE, Hue);
			SetCtrlVal(panelHandle, PANEL_SAT, Saturation);
			SetCtrlVal(panelHandle, PANEL_INT, Intensity);
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			break;
	}
	return 0;
}
