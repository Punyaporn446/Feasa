/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (blinking LED)
*
*  DESCRIPTION: This example demonstrates how to use Sequence
*  functions provided in the DLL to test a blinking LED
*  so that the light pattern could be tracked.
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
#include "SequenceBlinking.h"
#include "toolbox.h"
#include "feasacom.h"

#define TOFLASH 0 //Selects whether parameters are stored in flash (1) or RAM (0)

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
    errChk (panelHandle = LoadPanel (0, "SequenceBlinking.uir", PANEL));
	
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
	
	
	//Add options to list
	j=0;  
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "VERY LOW (<1Hz)", 0);
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "LOW (1-3Hz)", 2);
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "MEDIUM (3-10Hz)", 4);
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "MODERATE (10-20Hz)", 6);
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "HIGH (20-40Hz)", 8);
	InsertListItem(panelHandle, PANEL_LSTSIGNALSPEED, j++, "VERY HIGH (>40Hz)", 10);
	SetCtrlIndex(panelHandle, PANEL_LSTSIGNALSPEED, 3);
	
	//Add options to list
	j=0;  
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "0: VERY LOW", 0);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "1: VERY LOW", 1);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "2: LOW", 2);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "3: LOW", 3);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "4: MEDIUM", 4);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "5: MEDIUM", 5);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "6: MODERATE (fast blinking)", 6);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "7: MODERATE (very fast blinking)", 7);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "8: HIGH (can barely see it)", 8);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "9: HIGH", 9);
	InsertListItem(panelHandle, PANEL_LSTBLINKINGSPEED, j++, "10: HIGH (can't see it)", 10);
	SetCtrlIndex(panelHandle, PANEL_LSTBLINKINGSPEED, 6);
	
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


int CVICALLBACK btnFindParams (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int i;
	int ComPort;
	char buffer[300];
	int SignalSpeed = 0;
	int BlinkingSpeed = 0;
	int CycleCount = 0;
	int resp;
	int TimeResolutionIsImportant = 0;
	int CaptureTime = 0;
	int WaitTime = 0;
	int SampleCount = 0;
	int TotalLEDCount = 0;
	int Fiber = 0;

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:

			// Check if ComPort is selected
			GetNumListItems(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i==0) {
				MessagePopup("Information", "No Analysers detected!");
				return 0;
			}
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i<0) {
				MessagePopup("Information", "Please, select a port from the list");
				return 0;
			}
			
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &ComPort);
			
			GetCtrlVal(panelHandle, PANEL_LSTSIGNALSPEED, &SignalSpeed);
			GetCtrlVal(panelHandle, PANEL_LSTBLINKINGSPEED, &BlinkingSpeed);
			GetCtrlVal(panelHandle, PANEL_TXTCYCLES, &CycleCount);
			GetCtrlVal(panelHandle, PANEL_TXTLEDCOUNT, &TotalLEDCount);
			GetCtrlVal(panelHandle, PANEL_TXTFIBERFIND, &Fiber);
			GetCtrlVal(panelHandle, PANEL_CHKTIMERESIMPORTANT, &TimeResolutionIsImportant);
			
			if (FeasaCom_Open(ComPort, "57600") == 1)
			{
			
				//Increase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
				FeasaCom_SetResponseTimeout(8000); //8000 milliseconds
				
				 // Find out test settings
				resp = FeasaCom_Sequence_FindTestSettings(ComPort, TotalLEDCount, Fiber, SignalSpeed, BlinkingSpeed, CycleCount, TimeResolutionIsImportant, &CaptureTime, &WaitTime, &SampleCount);
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
			
				// Set results
				SetCtrlVal(panelHandle, PANEL_TXTCAPTURETIME, CaptureTime);
				SetCtrlVal(panelHandle, PANEL_TXTWAITTIME, WaitTime);
				SetCtrlVal(panelHandle, PANEL_TXTSAMPLES, SampleCount);
				
				// Close port
				FeasaCom_Close(ComPort);
				
				MessagePopup("Information", "Parameters calculated successfully!");
			}
			else
			{
				//Error: unable to open the selected port
				MessagePopup("Information", "Unable to open selected port");
			}
			
			
			break;
	}
	return 0;
}




int CVICALLBACK btnCaptureSequence (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int i;
	int ComPort;
	char buffer[300];
	int CycleCount = 0;
	int resp;
	int CaptureTime = 0;
	int WaitTime = 0;
	int SampleCount = 0;
	int TotalLEDCount = 0;
	int Fiber = 0;
	int StartDelay = 0;
	float xValues[9999];
	float yValues[9999];
	int IntensityValues[9999];

	switch (event)
	{
		case EVENT_COMMIT:

			break;
		case EVENT_LEFT_CLICK:
			
			// Check if ComPort is selected
			GetNumListItems(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i==0) {
				MessagePopup("Information", "No Analysers detected!");
				return 0;
			}
			GetCtrlIndex(panelHandle, PANEL_LSTCOMMPORT, &i);
			if (i<0) {
				MessagePopup("Information", "Please, select a port from the list");
				return 0;
			}
			//Retrieve port
			GetCtrlVal(panelHandle, PANEL_LSTCOMMPORT, &ComPort);
			
			//Clear any previous plot
			DeleteGraphPlot(panelHandle, PANEL_GRAPHINT, -1, VAL_IMMEDIATE_DRAW);
			DeleteGraphPlot(panelHandle, PANEL_GRAPHCIE, -1, VAL_IMMEDIATE_DRAW);
			
			//Retrieve test parameters
			GetCtrlVal(panelHandle, PANEL_TXTFIBER, &Fiber);
			GetCtrlVal(panelHandle, PANEL_TXTSTARTDELAY, &StartDelay);
			GetCtrlVal(panelHandle, PANEL_TXTCAPTURETIME, &CaptureTime);
			GetCtrlVal(panelHandle, PANEL_TXTWAITTIME, &WaitTime);
			GetCtrlVal(panelHandle, PANEL_TXTSAMPLES, &SampleCount);
			
			if (FeasaCom_Open(ComPort, "57600") == 1)
			{
			
				//Increase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
				FeasaCom_SetResponseTimeout(8000); //8000 milliseconds
				
				// setup test
				resp = FeasaCom_Sequence_Setup(ComPort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH);
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
				
				// Perform sequence capture
				resp = FeasaCom_Sequence_Capture(ComPort, Fiber);
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
				
				// Perform sequence capture
				resp = FeasaCom_Sequence_Capture(ComPort, Fiber);
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
				
				// Read back results
				resp = FeasaCom_Sequence_ReadxyI(ComPort, Fiber, xValues, yValues, IntensityValues);
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
				
				
				//Plots graph
				PlotWaveform(panelHandle, PANEL_GRAPHINT, IntensityValues, SampleCount, VAL_INTEGER, 1.0, 0.0, 0.0, 1.0, VAL_THIN_LINE, VAL_CONNECTED_POINTS, VAL_SOLID, 1, VAL_YELLOW);
				
				SetAxisScalingMode(panelHandle, PANEL_GRAPHCIE, VAL_LEFT_YAXIS, VAL_MANUAL, 0.0, 1.0);
				SetAxisScalingMode(panelHandle, PANEL_GRAPHCIE, VAL_RIGHT_YAXIS, VAL_MANUAL, 0.0, 1.0);
				PlotWaveform(panelHandle, PANEL_GRAPHCIE, xValues, SampleCount, VAL_FLOAT, 1.0, 0.0, 0.0, 1.0, VAL_THIN_LINE, VAL_CONNECTED_POINTS, VAL_SOLID, 1, VAL_RED);
				//Sleep(1000);
				PlotWaveform(panelHandle, PANEL_GRAPHCIE, yValues, SampleCount, VAL_FLOAT, 1.0, 0.0, 0.0, 1.0, VAL_THIN_LINE, VAL_CONNECTED_POINTS, VAL_SOLID, 1, VAL_WHITE);

				FeasaCom_Close(ComPort);
			}
			else
			{
				//Error: unable to open the selected port
				MessagePopup("Information", "Unable to open selected port");
			}
			
			break;
	}
	return 0;
}
