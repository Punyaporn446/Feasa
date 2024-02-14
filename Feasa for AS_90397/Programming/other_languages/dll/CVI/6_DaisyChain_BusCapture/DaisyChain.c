/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain (Bus capture)
*
*  DESCRIPTION: This example demonstrates how to perform
*  a capture for all Daisy-chained analysers, through the DLL
*  functions and then retrieve the HSI values for the fiber
*  requested.
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
#include "DaisyChain.h"
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
    errChk (panelHandle = LoadPanel (0, "DaisyChain.uir", PANEL));
	
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
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTUREMODE, 0);
	
	//Fill list with cature ranges
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 0, "LOW", 1);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 1, "MEDIUM", 2);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 2, "HIGH", 3);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 3, "SUPER", 4);
	InsertListItem(panelHandle, PANEL_LSTCAPTURERANGE, 4, "ULTRA", 5);
	SetCtrlIndex(panelHandle, PANEL_LSTCAPTURERANGE, 2);
	
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

int CVICALLBACK btnAddAnalyser (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	char sn[10];
	
	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			//Ask for the Serial of the Analyser to Add
			PromptPopup("Add Analyser", "Please, type the Serial number of the LED Analyser to add to the Bus:", sn, 4);
			if ( strlen(sn)!=4 ) {
				MessagePopup("Information", "Please, type a valid Serial number");
				return 0;
			}
			//Add item to the list
			InsertListItem(panelHandle, PANEL_LSTANALYSERS, 0, sn);
			break;
	}
	
	return 0;
}

int CVICALLBACK btnCaptureAndRead (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	
	char buffer[1024]; //Input Buffer used to communicate with the LED Analyser
					   //Ensure that it has enought size to fit all the data sent by the longest command used
	int port; //holds the port to be opened
	int nfib; //Auxiliar integer
	int r; //holds the response from the DLL commands
	char command[100]; //Auxiliar string used to compose commands
	char sn_main[20];
	char aux[10];
	int CaptureRange;
	int i, n;

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			//Get selected port
			port = -1;
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i<0) {
				MessagePopup("Information", "Please, select a port first.");
				return 0;
			}
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
		
			GetNumListItems(panelHandle, PANEL_LSTANALYSERS, &n);
			if (n==0) {
				MessagePopup("Information", "Please, add Analysers to the bus first.");
				return 0;
			}
			
			//Clear Results box
			ResetTextBox (panelHandle, PANEL_TXTRESULT, "");
			
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
			
			//Add Daisy Chain Devices
			for (i=0; i<n; i++) {
				//Get Serial Number for this Analyser
				GetLabelFromIndex(panelHandle, PANEL_LSTANALYSERS, i, aux);
				//Add device
				r = FeasaCom_DaisyChain_Add(port, aux);
				if ( r==0 ) {
					MessagePopup("Error", "Unable to add Analyser to the BUS!");
					FeasaCom_Close(port);
					return 0;
				}
			}

			//Compose Capture command
			GetCtrlIndex(panelHandle, PANEL_LSTCAPTUREMODE, &i);
			if (i==0) {
				CaptureRange = 0;
			} else if (i==1) {
				GetCtrlVal(panelHandle, PANEL_LSTCAPTURERANGE, &CaptureRange);
			}
			
			//Execute a capture in the Feasa LED Analyser
			r = FeasaCom_DaisyChain_Capture(port, 0, CaptureRange, 0);
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Free the Bus
			r = FeasaCom_Send(port, "BUSFREE", &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Read Serial Number of main/master Analyser
			r = FeasaCom_Send(port, "GETSERIAL", &sn_main[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			//Get Fiber number
			
			GetCtrlVal(panelHandle, PANEL_NUMFIBER, &nfib);
			
			//Read back measurements from the main Analyser
			sprintf(command, "GETHSI%.2d", nfib); //compose command
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
			//Shows result in the textbox
			sprintf(command, "Fib %02d(%s): %s\n", nfib, sn_main, buffer);
			SetCtrlVal(panelHandle, PANEL_TXTRESULT, &command);
			
			for (i=0; i<n; i++) {
				
				//Get Serial Number for this Analyser
				GetLabelFromIndex(panelHandle, PANEL_LSTANALYSERS, i, aux);
				
				//Set the bus
				sprintf(command, "BUSGET%s", &aux); //compose command
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
				
				//Read back measurements from the main Analyser
				sprintf(command, "GETHSI%.2d", nfib); //compose command
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
				
				//Shows result in the textbox
				sprintf(command, "Fib %02d(%s): %s\n", nfib, aux, buffer);
				/*GetCtrlVal(panelHandle, PANEL_TXTRESULT, &buffer);
				strcat(buffer, command); */
				SetCtrlVal(panelHandle, PANEL_TXTRESULT, &command);
			}   
			
			//Free the Bus
			r = FeasaCom_Send(port, "BUSFREE", &buffer[0]); // This function sends a command to the LED Analyser. CR and LF characters are always automatically added to any command sent
			if ( r==-1 ) {
				MessagePopup("Error", "Unable to send the command!.");
				FeasaCom_Close(port);
				return 0;
			} else if ( r==0 ) {
				MessagePopup("Error", "Syntax Error or Timeout detected.");
				FeasaCom_Close(port);
				return 0;
			}
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			break;
	}
	return 0;
}
