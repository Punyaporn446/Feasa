/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (sweeping light)
*
*  DESCRIPTION: This example demonstrates how To use Sequence
*  functions provided In the DLL To test a sweeping light
*  pattern from different LEDs, extracting intensity and
*  pattern times afterwards.
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
#include "SequenceSweeping.h"
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
    errChk (panelHandle = LoadPanel (0, "SequenceSweeping.uir", PANEL));
	
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



int CVICALLBACK btnCaptureSequence (int panel, int control, int event,
		void *callbackData, int eventData1, int eventData2)
{
	int i;
	int j;
	int ComPort;
	char buffer[300];
	int CycleCount = 0;
	int resp;
	int CaptureTime = 0;
	int WaitTime = 0;
	int SampleCount = 0;
	int FibersToTest = 0;
	int StartDelay = 0;
	int IntensityValues[9999];
	double OffsetInt = 0;
	int MaxIntensity = 0;
	int isOffToOnPattern = 0;
    int LowTimes[20];
    int HighTimes[20];
    int tIntensityValues[20];

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
			
			//Retrieve test parameters
			GetCtrlVal(panelHandle, PANEL_TXTFIBERSTOTEST, &FibersToTest);
			GetCtrlVal(panelHandle, PANEL_TXTSTARTDELAY, &StartDelay);
			GetCtrlVal(panelHandle, PANEL_TXTCAPTURETIME, &CaptureTime);
			GetCtrlVal(panelHandle, PANEL_TXTWAITTIME, &WaitTime);
			GetCtrlVal(panelHandle, PANEL_TXTSAMPLES, &SampleCount);
			GetCtrlVal(panelHandle, PANEL_CHKISOFFTOON, &isOffToOnPattern);
			
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
				resp = FeasaCom_Sequence_Capture(ComPort, 0); //0: test all fibers
				if (resp != 1) {
					FeasaCom_GetError_Description(buffer);
					MessagePopup("Error!", buffer);
					FeasaCom_Close(ComPort);
					return 1;
				}
				
				
				for (i=1; i<=FibersToTest; i++) {
					
					// Read back results
					resp = FeasaCom_Sequence_ReadIntensity(ComPort, i, IntensityValues);
					if (resp != 1) {
						FeasaCom_GetError_Description(buffer);
						MessagePopup("Error!", buffer);
						FeasaCom_Close(ComPort);
						return 1;
					}
					
					MaxIntensity = 0;
					for (j=0; j<SampleCount; j++) {
						if (IntensityValues[j]>MaxIntensity) MaxIntensity = IntensityValues[j];
						IntensityValues[j] += OffsetInt;
					}
					
					OffsetInt += (double)MaxIntensity * 1.1;
				
					//Plots graph
					if ((i%2)==0)
						PlotWaveform(panelHandle, PANEL_GRAPHINT, IntensityValues, SampleCount, VAL_INTEGER, 1.0, 0.0, 0.0, 1.0, VAL_THIN_LINE, VAL_CONNECTED_POINTS, VAL_SOLID, 1, VAL_YELLOW);
					else
						PlotWaveform(panelHandle, PANEL_GRAPHINT, IntensityValues, SampleCount, VAL_INTEGER, 1.0, 0.0, 0.0, 1.0, VAL_THIN_LINE, VAL_CONNECTED_POINTS, VAL_SOLID, 1, VAL_RED);
				}
				
				if (OffsetInt>0) {
					SetAxisScalingMode(panelHandle, PANEL_GRAPHINT, VAL_LEFT_YAXIS, VAL_MANUAL, 0.0, OffsetInt);
					SetAxisScalingMode(panelHandle, PANEL_GRAPHINT, VAL_RIGHT_YAXIS, VAL_MANUAL, 0.0, OffsetInt);
				}
				
				// Retrieve LED times
                resp = FeasaCom_Sequence_GetSweepingPattern(ComPort, FibersToTest, isOffToOnPattern, LowTimes, HighTimes, tIntensityValues);
                if (resp != 1)
                {
                    FeasaCom_GetError_Description(buffer);
						MessagePopup("Error!", buffer);
						FeasaCom_Close(ComPort);
						return 1;
                }
				
				//Clear table
				GetNumTableRows(panelHandle, PANEL_TABLETIMING, &i);
				if (i>0)
					DeleteTableRows(panelHandle, PANEL_TABLETIMING, 0, -1);
				
				//Fill in table with times
				for (i=0; i<FibersToTest; i++) {
					InsertTableRows(panelHandle, PANEL_TABLETIMING, -1, 1, VAL_CELL_NUMERIC);
					SetTableCellVal(panelHandle, PANEL_TABLETIMING, MakePoint(1, i + 1), (double)(i+1));
					SetTableCellVal(panelHandle, PANEL_TABLETIMING, MakePoint(2, i + 1), (double)LowTimes[i]);
					SetTableCellVal(panelHandle, PANEL_TABLETIMING, MakePoint(3, i + 1), (double)HighTimes[i]);
					SetTableCellVal(panelHandle, PANEL_TABLETIMING, MakePoint(4, i + 1), (double)tIntensityValues[i]);
				}														   

				//Close port
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
