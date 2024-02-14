/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: baudrate
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser picking a
*  known baudrate or detecting it automatically.
*
*  Important Note: it is not possible to communicate to a
*  LED Analyser that does not have axactly the same
*  baudrate used to open the port.
*  Factory default: 57600 baud.
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
#include "baudrate.h"
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
    errChk (panelHandle = LoadPanel (0, "baudrate.uir", PANEL));
	Radio_ConvertFromTree (panelHandle, PANEL_OPTBAUDRATE);
	Radio_ConvertFromTree (panelHandle, PANEL_OPTBAUDRATE);
	
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
	
	//Add baudrates
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 0, "9600", 9600);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 1, "19200", 19200);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 2, "38400", 38400);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 3, "57600", 57600);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 4, "115200", 115200);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 5, "230400", 230400);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 6, "460800", 460800);
	InsertListItem(panelHandle, PANEL_LSTBAUDRATE, 7, "921600", 921600);
	SetCtrlIndex(panelHandle, PANEL_LSTBAUDRATE, 3);
	
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
	char command[50]; //Auxiliary string used to compose commands
	char aux[100]; //Auxiliar string
	clock_t tIni;
	clock_t tEnd;

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
		
			ResetTextBox(panelHandle, PANEL_TXTLOG, "");
			
			//This command enumerates the existing ports to find out what are the serial
			// ports available on your computer and the devices connected to them.
			// This function needs to be executed any time that a Feasa device is
			// connected or disconnected, while the application is running
			//FeasaCom_EnumPorts();
			
			tIni = clock();
			
			Radio_GetMarkedOption(panelHandle, PANEL_OPTBAUDRATE, &auxint);
			if (auxint==0) {
				//Open the port previously selected, where it's supposed the LED Analyser to be.
				if ( FeasaCom_Open(port, "AUTO")==0 ) {
					MessagePopup("Error", "Unable to open port!");
					return 0;
				}
				auxint = FeasaCom_GetBaudrate(port);
				sprintf(aux, "Current Baudrate: %d\r\n", auxint);
				SetCtrlVal(panelHandle, PANEL_TXTLOG, aux);
			} else {
				GetCtrlVal(panelHandle, PANEL_LSTBAUDRATE, &auxint);
				sprintf(aux, "%d", auxint);
				//Open the port previously selected, where it's supposed the LED Analyser to be.
				if ( FeasaCom_Open(port, aux)==0 ) {
					MessagePopup("Error", "Unable to open port!");
					return 0;
				}
			}
			
			//Log
			SetCtrlVal(panelHandle, PANEL_TXTLOG, "Capture: ");
			
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
			
			//Log
			SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
			SetCtrlVal(panelHandle, PANEL_TXTLOG, "\r\n");
			
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
			
			//Log
			SetCtrlVal(panelHandle, PANEL_TXTLOG, buffer);
			SetCtrlVal(panelHandle, PANEL_TXTLOG, "\r\n");
			
			
			//Close communications port
			FeasaCom_Close(port);
			
			//Calculates elapsed time
			tEnd = clock();
			double elapsed = ((double)(tEnd - tIni) / (double)(CLOCKS_PER_SEC)) * 1000.0;
			sprintf(aux, "Elapsed: %0.2f ms", elapsed);
			SetCtrlVal(panelHandle, PANEL_TXTLOG, aux);
			
			break;
	}
	return 0;
}
