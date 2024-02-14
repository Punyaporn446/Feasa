/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Search & Detect
*
*  DESCRIPTION: This example demonstrates how to list
*  all available Feasa devices, and also to locate
*  the port number of a connected Device based on its serial
*  number.
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
#include <time.h>
#include "radioGroup.h"
#include "search_detect.h"
#include "toolbox.h"
#include "feasacom.h"

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
    errChk (panelHandle = LoadPanel (0, "search_detect.uir", PANEL));
	
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

int CVICALLBACK btnAction (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	
	char buffer[1024]; //Input Buffer used to communicate with the LED Analyser
					   //Ensure that it has enought size to fit all the data sent by the longest command used
	int port; //Holds port number to be opened
	int r; //holds the response from the DLL commands
	int i; //aux var
	char aux[100]; //auxiliary variable
	int Ports[255]; //Holds list of ports of devices detected

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			
			if ( control==PANEL_BTNFIND )
			{
				//Clear log
				ResetTextBox(panelHandle, PANEL_TXTLOG, "");
				
				//Get selected port
				port = -1;
				GetCtrlVal(panelHandle, PANEL_TXTSN, aux);
				if (strlen(aux)<4) {
					MessagePopup("Information", "Please, write a valid Serial Number.");
					return 0;
				}
				
				port = FeasaCom_IsConnected(aux, "AUTO");
				if (port<=0) {
					MessagePopup("Information", "Device not found!");
					return 0;
				}
			
				//Log
				sprintf(aux, "LED Analyser found on port %d", port);
				SetCtrlVal(panelHandle, PANEL_TXTLOG, aux);
				
			
				//This command enumerates the existing ports to find out what are the serial
				// ports available on your computer and the devices connected to them.
				// This function needs to be executed any time that a Feasa device is
				// connected or disconnected, while the application is running
				FeasaCom_EnumPorts();
			
				//Open the port previously selected, where it's supposed the LED Analyser to be.
				if ( FeasaCom_Open(port, "AUTO")==0 ) {
					MessagePopup("Error", "Unable to open port!");
					return 0;
				}
			
				//Execute a capture in the Feasa LED Analyser
				r = FeasaCom_Send(port, "GETSTATUS", &buffer[0]);
				if ( r==-1 ) {
					MessagePopup("Error", "Unable to send the command!");
					FeasaCom_Close(port);
					return 0;
				} else if ( r==0 ) {
					MessagePopup("Error", "Syntax Error or Timeout detected.");
					FeasaCom_Close(port);
					return 0;
				}
			
				//Log
				SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
				SetCtrlVal(panelHandle, PANEL_TXTLOG, "\r\n");
			
			
				//Close communications port
				FeasaCom_Close(port);
			
			}
			else if ( control==PANEL_BTNDETECT )
			{
				//Clear log
				ResetTextBox(panelHandle, PANEL_TXTLOG, "");
				
				//This command enumerates the existing ports to find out what are the serial
				// ports available on your computer and the devices connected to them.
				// This function needs to be executed any time that a Feasa device is
				// connected or disconnected, while the application is running
				FeasaCom_EnumPorts();
				
				r = FeasaCom_Detect(Ports, "AUTO");
				if (r==0) {
					MessagePopup("Error", "No devices detected!");
					return 0;
				} else {
					SetCtrlVal(panelHandle, PANEL_TXTLOG, "Devices detected\r\n");
					for (i=0; i<r; i++) {
						sprintf(aux, "...on port COM%d\r\n", Ports[i]);
						SetCtrlVal(panelHandle, PANEL_TXTLOG, aux);
					}
				}
			}
			break;
	}
	return 0;
}
