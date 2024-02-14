/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Fast Test (Multi-threaded)
*
*  DESCRIPTION: This example demonstrates how to use the multi-
*  threaded functions provided in the DLL to set up a fast and
*  efficient communication schema for your application.
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
**************************************************************/

//==============================================================================
// Include files

#include <ansi_c.h>
#include <cvirte.h>     
#include <userint.h>
#include <windows.h>
#include <time.h>
#include "FastTest.h"
#include "toolbox.h"
#include "feasacom.h"
#include "feasa_tools.h"

#define MAXDEVICES 10 //Maximum number of allowed devices to be tested

// Static global variables
static int panelHandle;
int Ports[MAXDEVICES];
int nPorts;

void FreeArrayOfStrings(char ** mArrayOfStrings, int Size)
{
	int i;
	for (i = 0; i < Size; i++)
		free(mArrayOfStrings[i]);
	free(mArrayOfStrings);
}

//-------------- MAIN ---------------------------------------------------------
int main (int argc, char *argv[])
{
    int error = 0;
	int i, j;
	char aux[50];
	
    /* initialize and load resources */
    nullChk (InitCVIRTE (0, argv, 0));
    errChk (panelHandle = LoadPanel (0, "FastTest.uir", PANEL));
	
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
	
	nPorts = 0;
	
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
	int n;
	int port;
	char aux[10];
	int i;
	
	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i<0) {
				MessagePopup("Information", "Please, select a port from the list");
				return 0;
			}
			
			GetNumListItems(panelHandle, PANEL_LSTCOMMPORT, &n);
			if (n==0) {
				MessagePopup("Information", "No Analysers detected!");
				return 0;
			}
			
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &port);
			
			//Add item to the list
			sprintf(aux, "COM %d", port);
			InsertListItem(panelHandle, PANEL_LSTPORTSTOTEST, 0, aux, port);
			
			//Remove item from the list
			DeleteListItem(panelHandle, PANEL_LSTCOMMPORT, i, 1);
			
			Ports[nPorts++] = port;
			
			break;
	}
	
	return 0;
}

int CVICALLBACK btnCaptureAndRead (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	char baudrate[20]; //Used to store the baudrate selected
	int i; // auxiliar
	char ** Serials = NULL;
	char ** Responses = NULL;
	int ReturnValues[MAXDEVICES];
	float HueValues[20];
	int SaturationValues[20];
	int IntensityValues[20];
	clock_t tIni;
	clock_t tEnd;

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:

			//Get selected port
			if (nPorts==0) {
				MessagePopup("Information", "Please, add at least one port to the list of devices to be tested.");
				return 0;
			}
			
			//Clear Results box
			ResetTextBox (panelHandle, PANEL_TXTRESULTS, "");
			
			//Increase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
			FeasaCom_SetResponseTimeout(8000); //8000 milliseconds
			
			//Initialize array of strings
			Serials = (char **)malloc(sizeof(char *) * MAXDEVICES);
			Responses = (char **)malloc(sizeof(char *) * MAXDEVICES);
			for (i = 0; i < MAXDEVICES; i++) {
				Serials[i] = (char *)malloc(sizeof(char) * 100 + 1);
				Responses[i] = (char *)malloc(sizeof(char) * 500 + 1);
			}
			
			tIni = clock();
			
			if (FeasaCom_Open_Multi(ReturnValues, Ports, nPorts, "57600") == 1)
			{
				//No error

				//Retrieve Serial numbers connected
				for (int i = 0; i < nPorts; i++)
				{
					resp = FeasaCom_GetSNByPort(buffer, Ports[i]);
					if (resp == 1)
						strcpy(Serials[i], buffer);
					else
						Serials[i] = '\0';
				}

				//Send command to All Analysers connected
				resp = FeasaCom_SendToAll(ReturnValues, "CAPTURE", Responses);
				if (resp != 1)
				{
					for (int i = 0; i < nPorts; i++)
						if (ReturnValues[i] == -1)
						{
							sprintf(buffer, "Unable to send the command to %s!", Serials[i]);
							MessagePopup("Information", buffer);
							FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
							FreeArrayOfStrings(Serials, MAXDEVICES);
							FreeArrayOfStrings(Responses, MAXDEVICES);
							return 1;
						}
						else if (ReturnValues[i] == 0)
						{
							sprintf(buffer, "Timeout or Syntax error detected in %s", Serials[i]);
							MessagePopup("Information", buffer);
							FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
							FreeArrayOfStrings(Serials, MAXDEVICES);
							FreeArrayOfStrings(Responses, MAXDEVICES);
							return 1;
						}
				}

				//Send command to All Analysers connected
				resp = FeasaCom_SendToAll(ReturnValues, "GETHSIALL", Responses);
				if (resp != 1)
				{
					for (int i = 0; i < nPorts; i++)
						if (ReturnValues[i] == -1)
						{
							sprintf(buffer, "Unable to send the command to %s!", Serials[i]);
							MessagePopup("Information", buffer);
							FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
							FreeArrayOfStrings(Serials, MAXDEVICES);
							FreeArrayOfStrings(Responses, MAXDEVICES);
							return 1;
						}
						else if (ReturnValues[i] == 0)
						{
							sprintf(buffer, "Timeout or Syntax error detected in %s!", Serials[i]);
							MessagePopup("Information", buffer);
							FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
							FreeArrayOfStrings(Serials, MAXDEVICES);
							FreeArrayOfStrings(Responses, MAXDEVICES);
							return 1;
						}
				}

				//Extract response lines and parse responses
				for (int i = 0; i < nPorts; i++)
				{
					//Declare numerical arrays for responses with 20 elements (maximum number of channels)
					int nLines = Feasa_Parse_HSI_All(Responses[i], HueValues, SaturationValues, IntensityValues);
					if (nLines > 0)
					{
						for (int f = 0; f < nLines; f++) {
							sprintf(buffer, "%s -> %02d  %03.2f  %03d  %05d\n", Serials[i], f + 1, HueValues[f], SaturationValues[f], IntensityValues[f]);
							SetCtrlVal(panelHandle, PANEL_TXTRESULTS, buffer);
						}
					}
				}

				//Close the port
				FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
			}
			else
			{
				//Error: unable to open the selected port
				MessagePopup("Information", "Unable to open all ports");
			}
			
			tEnd = clock();
			double elapsed = (tEnd - tIni)  * 1000.0;
			elapsed = elapsed / (double)CLOCKS_PER_SEC;
			sprintf(buffer, "Elapsed time: %0.2f ms!\n", elapsed);
			SetCtrlVal(panelHandle, PANEL_TXTEXECTIME, buffer);
			
			FreeArrayOfStrings(Serials, MAXDEVICES);
			FreeArrayOfStrings(Responses, MAXDEVICES);
			
			break;
	}
	return 0;
}
