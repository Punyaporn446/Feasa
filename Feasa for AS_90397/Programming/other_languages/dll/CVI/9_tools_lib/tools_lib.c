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

//==============================================================================
// Include files

#include <ansi_c.h>
#include <cvirte.h>     
#include <userint.h>
#include <windows.h>
#include "tools_lib.h"
#include "toolbox.h"
#include "feasacom.h"
#include "feasa_tools.h"

// Static global variables

static int panelHandle;

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
    errChk (panelHandle = LoadPanel (0, "tools_lib.uir", PANEL));
	
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
	char command[50]; //Auxiliar string used to compose commands
	unsigned char Red, Green, Blue; //Used to store the RGB data
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
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &r);
			if (r<0) {
				MessagePopup("Information", "Please, select a port first.");
				return 0;
			}
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
		
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
			
			//Execute a capture in the Feasa LED Analyser
			r = FeasaCom_Send(port, "CAPTURE", &buffer[0]);
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Read back Color and Intensity measurements
			GetCtrlVal(panelHandle, PANEL_NUMFIBER, &auxint);
			sprintf(command, "GETRGBI%.2d", auxint); //compose command
			r = FeasaCom_Send(port, command, &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Parse and format the response
			Feasa_Parse_RGBI(buffer, &Red, &Green, &Blue, &Intensity);
			SetCtrlVal(panelHandle, PANEL_RED, Red);
			SetCtrlVal(panelHandle, PANEL_GREEN, Green);
			SetCtrlVal(panelHandle, PANEL_BLUE, Blue);
			SetCtrlVal(panelHandle, PANEL_INT, Intensity);
			
			
			//Read back Color and Intensity measurements
			GetCtrlVal(panelHandle, PANEL_NUMFIBER, &auxint);
			sprintf(command, "GETHSI%.2d", auxint); //compose command
			r = FeasaCom_Send(port, command, &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Parse and format the response
			Feasa_Parse_HSI(buffer, &Hue, &Saturation, &Intensity);
			SetCtrlVal(panelHandle, PANEL_HUE, Hue);
			SetCtrlVal(panelHandle, PANEL_SAT, Saturation);
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			break;
	}
	return 0;
}
