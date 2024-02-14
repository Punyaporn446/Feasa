/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence
*
*  DESCRIPTION: This example demonstrates how to use Sequence
*  functions provided in the DLL to test a blinking LED and
*  a sweeping light pattern made by several LEDs so that the
*  light pattern could be tracked.
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

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <time.h>
#include "feasacom.h"

#define TOFLASH 0 //Decides whether adjustments are stored in flash or RAM

// Declare pointers to call functions
tFeasaCom_IsPortAvailable FeasaCom_IsPortAvailable;
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;
tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;
tFeasaCom_GetBaudrate FeasaCom_GetBaudrate;
tFeasaCom_GetError_Description FeasaCom_GetError_Description;
tFeasaCom_Sequence_Setup FeasaCom_Sequence_Setup;
tFeasaCom_Sequence_Capture FeasaCom_Sequence_Capture;
tFeasaCom_Sequence_ReadIntensity FeasaCom_Sequence_ReadIntensity;
tFeasaCom_Sequence_ReadxyI FeasaCom_Sequence_ReadxyI;
tFeasaCom_Sequence_ReadHSI FeasaCom_Sequence_ReadHSI;
tFeasaCom_Sequence_GetPattern FeasaCom_Sequence_GetPattern;
tFeasaCom_Sequence_GetSweepingPattern FeasaCom_Sequence_GetSweepingPattern;
tFeasaCom_Sequence_GetFrequency FeasaCom_Sequence_GetFrequency;
tFeasaCom_Sequence_FindTestSettings FeasaCom_Sequence_FindTestSettings;

void FormatDecimal(char * buffer);
int FindParamaters(int CommPort, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber);
int SequenceTest(int CommPort, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber);
int SweepingTest(int CommPort, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount);

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char command[100]; // Used to store the composed command
	int CommPort = 6;
	int NumFibers = 5;
	char Baudrate[20] = "AUTO";
	int i; // auxiliar
	int CaptureTime = 0; // to store Capture time
	int WaitTime = 0; // to store Time between captures
	int SampleCount = 0; // to store Number of samples
	int TotalLEDCount = 0; // to stores Total numbers of LEDs to be tested simultaneously
	int Fiber = 0; //To store Fiber/channel to be used for sequence (0 for all)
	int StartDelay = 0; //To store initial delay before start capturing the sequence

	/* --- DLL ----------------------------------------------------------------*/
	// Load
	printf("Loading DLL...");
	HINSTANCE hFeasaComDLL=LoadLibrary("feasacom.dll");
	if(hFeasaComDLL==NULL) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of DLL functions
	printf("Getting Function addresses...");
	FeasaCom_IsPortAvailable = (tFeasaCom_IsPortAvailable)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_IsPortAvailable");
	FeasaCom_Open = (tFeasaCom_Open)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Open");
	FeasaCom_Send = (tFeasaCom_Send)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send");
	FeasaCom_Close = (tFeasaCom_Close)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Close");
	FeasaCom_GetBaudrate = (tFeasaCom_GetBaudrate)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetBaudrate");
	FeasaCom_SetResponseTimeout = (tFeasaCom_SetResponseTimeout)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SetResponseTimeout");
	FeasaCom_GetError_Description = (tFeasaCom_GetError_Description)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetError_Description");
	FeasaCom_Sequence_Setup = (tFeasaCom_Sequence_Setup)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_Setup");
	FeasaCom_Sequence_Capture = (tFeasaCom_Sequence_Capture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_Capture");
	FeasaCom_Sequence_ReadIntensity = (tFeasaCom_Sequence_ReadIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadIntensity");
	FeasaCom_Sequence_ReadxyI = (tFeasaCom_Sequence_ReadxyI)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadxyI");
	FeasaCom_Sequence_ReadHSI = (tFeasaCom_Sequence_ReadHSI)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadHSI");
	FeasaCom_Sequence_GetPattern = (tFeasaCom_Sequence_GetPattern)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetPattern");
	FeasaCom_Sequence_GetSweepingPattern = (tFeasaCom_Sequence_GetSweepingPattern)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetSweepingPattern");
	FeasaCom_Sequence_GetFrequency = (tFeasaCom_Sequence_GetFrequency)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetFrequency");
	FeasaCom_Sequence_FindTestSettings = (tFeasaCom_Sequence_FindTestSettings)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_FindTestSettings");
	if ((FeasaCom_IsPortAvailable == NULL) || (FeasaCom_Open == NULL) || (FeasaCom_Send == NULL) || (FeasaCom_Close == NULL) || (FeasaCom_GetBaudrate == NULL) || (FeasaCom_SetResponseTimeout == NULL) || (FeasaCom_GetError_Description == NULL) || (FeasaCom_Sequence_Setup == NULL) || (FeasaCom_Sequence_Capture == NULL) || (FeasaCom_Sequence_ReadIntensity == NULL) || (FeasaCom_Sequence_ReadxyI == NULL) || (FeasaCom_Sequence_ReadHSI == NULL) || (FeasaCom_Sequence_GetPattern == NULL) || (FeasaCom_Sequence_GetSweepingPattern == NULL) || (FeasaCom_Sequence_GetFrequency == NULL) || (FeasaCom_Sequence_FindTestSettings == NULL)) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");
	/* ------------------------------------------------------------------------*/


	/*This command enumerates the existing ports to find out
    what are the serial ports available on your computer and
    the devices connected to them. You need to execute this
    command everytime you plug or unplug any Feasa Device,
    while the application is running */
    //FeasaCom_EnumPorts();

	//Increase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
	FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

	//Allow the user to choose the test settings
	printf("\nPlease, enter COM Port:\n");
	for (i = 1; i<100; i++)
		if (FeasaCom_IsPortAvailable(i) == 1)
			printf("  %d: COM%d\n", i, i);
	printf("  Your selection:");
	scanf("%d", &CommPort);
	if ((CommPort<1) || (CommPort>255)) CommPort = 1;

	//Allow the user to choose the test settings
	printf("\nPlease, choose Baudrate:\n");
	printf("  0: AUTO\n");
	printf("  1: 9600\n");
	printf("  2: 19200\n");
	printf("  3: 38400\n");
	printf("  4: 57600\n");
	printf("  5: 115200\n");
	printf("  6: 230400\n");
	printf("  7: 460800\n");
	printf("  8: 921600\n");
	printf("  Your selection:");
	scanf("%d", &i);
	if ( (i<0)||(i>5) ) i = 0;

	switch (i) {
		case 1: strcpy(Baudrate, "9600"); break;
		case 2: strcpy(Baudrate, "19200"); break;
		case 3: strcpy(Baudrate, "38400"); break;
		case 4: strcpy(Baudrate, "57600"); break;
		case 5: strcpy(Baudrate, "115200"); break;
		case 6: strcpy(Baudrate, "230400"); break;
		case 7: strcpy(Baudrate, "460800"); break;
		case 8: strcpy(Baudrate, "921600"); break;
		default: strcpy(Baudrate, "AUTO"); break;
	}

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!FeasaCom_Open(CommPort, Baudrate)) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");

	while (1) {
		//Allow the user to choose the test settings
		printf("\nPlease, choose an action:\n");
		printf("  1: FIND OUT SEQUENCE TEST PARAMETERS (for blinking or single pulse LEDs)\n");
		printf("  2: SEQUENCE TEST\n");
		printf("  3: SWEEPING TEST (for automotive sweeping indicators)\n");
		printf("  0: EXIT\n");
		printf("  Your selection:");
		scanf("%d", &i);
		
		if (i == 1) {
			FindParamaters(CommPort, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount, &Fiber);
		}
		else if (i == 2) {
			SequenceTest(CommPort, &StartDelay, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount, &Fiber);
		}
		else if (i == 3) {
			SweepingTest(CommPort, &StartDelay, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount);
		}
		else {
			break;
		}

	} //WHILE

	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(CommPort) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaComDLL);
	return 0;
}

int FindParamaters(int CommPort, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber)
{
	char buffer[300];
	int SignalSpeed;
	int BlinkingSpeed;
	int CycleCount;
	int TimeResolutionIsImportant;
	int resp;

	//Allow the user to choose the test settings
	printf("\nPlease, enter number of Fibers/LEDs to be tested simultaneously with the Sequence feature [1, 20]:");
	scanf("%d", TotalLEDCount);
	if ((*TotalLEDCount < 1) || (*TotalLEDCount > 20)) *TotalLEDCount = 20;

	printf("\nPlease, enter the Fiber to use to find the test parameters [1, 20]:");
	scanf("%d", Fiber);
	if ((*Fiber<1) || (*Fiber>20)) *Fiber = 20;

	printf("\nPlease, choose Signal Speed:\n");
	printf("  0: very low (<1Hz)\n");
	printf("  2: low (1-3Hz)\n");
	printf("  4: medium (3-10Hz)\n");
	printf("  6: moderate (10-20Hz)\n");
	printf("  8: high (20-40Hz)\n");
	printf("  10: very high (>40Hz)\n");
	printf("  Your selection [0, 10]: ");
	scanf("%d", &SignalSpeed);
	if ((SignalSpeed<0) || (SignalSpeed>10)) SignalSpeed = 5;

	printf("\nPlease, choose Blinking Speed:\n");
	printf("  0: very low\n");
	printf("  2: low\n");
	printf("  4: medium\n");
	printf("  6: moderate (fast)\n");
	printf("  8: high (very fast blinking)\n");
	printf("  10: very high (can't see it blinking)\n");
	printf("  Your selection [0, 10]: ");
	scanf("%d", &BlinkingSpeed);
	if ((BlinkingSpeed<0) || (BlinkingSpeed>10)) BlinkingSpeed = 5;

	printf("\nPlease, enter the number of Cycles to test: ");
	scanf("%d", &CycleCount);
	if ((CycleCount<1) || (CycleCount>20)) CycleCount = 5;

	printf("\nIs time resolution important?\n");
	printf("  0: no\n");
	printf("  1: yes\n");
	printf("  Your selection [0, 1]: ");
	scanf("%d", &TimeResolutionIsImportant);
	if ((TimeResolutionIsImportant<0) || (TimeResolutionIsImportant>1)) TimeResolutionIsImportant = 1;

	resp = FeasaCom_Sequence_FindTestSettings(CommPort, *TotalLEDCount, *Fiber, SignalSpeed, BlinkingSpeed, CycleCount, TimeResolutionIsImportant, CaptureTime, WaitTime, SampleCount);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("Error: %s\n", buffer);
		return 0;
	}

	printf("Results:\n");
	printf("CaptureTime=%d ms\n", *CaptureTime);
	printf("WaitTime=%d ms\n", *WaitTime);
	printf("SampleCount=%d\n", *SampleCount);
	printf("\n");

	return 1;
}

int SequenceTest(int CommPort, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber)
{
	char buffer[300];
	int UseDefaultParams = 0;
	int resp;
	float xValues[9999];
	float yValues[9999];
	int IntensityValues[9999];
	int PatternStatusCount;
	int PatternTimes[999];
	int PatternIntensities[999];
	int i, f;

	//Check if test parameters were previously initialized
	if (*SampleCount > 0) {
		printf("\nWould you like to use previous parameters?\n");
		printf("   StartDelay=%d ms\n", *StartDelay);
		if (*TotalLEDCount==1)
			printf("   Fiber=%d\n", *Fiber);
		else
			printf("   TotalLEDCount=%d\n", *TotalLEDCount);
		printf("   CaptureTime=%d ms\n", *CaptureTime);
		printf("   WaitTime=%d ms\n", *WaitTime);
		printf("   SampleCount=%d\n", *SampleCount);
		printf("  Your selection (yes=1, no=0): ");
		scanf("%d", &UseDefaultParams);
		if (UseDefaultParams!=1) UseDefaultParams = 0;
	}

	//If default parameters are not use or does not exists...
	if (UseDefaultParams != 1) {
		//Ask for test parameters
		printf("\nPlease, enter total number of LEDs/fibers to test simultaneously: ");
		scanf("%d", TotalLEDCount);
		if ((*TotalLEDCount<1) || (*TotalLEDCount>20)) *TotalLEDCount = 1;

		if (*TotalLEDCount == 1) {
			printf("\nPlease, enter Fiber to test: ");
			scanf("%d", Fiber);
			if ((*Fiber < 1) || (*Fiber > 20)) *Fiber = 1;
		}

		printf("\nPlease, enter initial delay before start Capturing [0, 999]ms: ");
		scanf("%d", StartDelay);
		if ((*StartDelay<0) || (*StartDelay>999)) *StartDelay = 0;

		printf("\nPlease, enter Capture time [1, 999]ms: ");
		scanf("%d", CaptureTime);
		if ((*CaptureTime < 1) || (*CaptureTime > 999)) *CaptureTime = 5;

		printf("\nPlease, enter Wait time (time between captures) [0, 999]ms: ");
		scanf("%d", WaitTime);
		if ((*WaitTime < 0) || (*WaitTime > 999)) *WaitTime = 0;

		if (*TotalLEDCount == 1)
			printf("\nPlease, enter number of samples [1, 9999]: ");
		else
			printf("\nPlease, enter number of samples [1, 3500]: ");
		scanf("%d", SampleCount);
		if (*SampleCount < 1) *SampleCount = 10;
		else if ((*SampleCount > 3500) && (*TotalLEDCount > 1)) *SampleCount = 100;
		else if ((*SampleCount > 9999) && (*TotalLEDCount == 1)) *SampleCount = 100;
	}

	printf("Setting up sequence... ");
	resp = FeasaCom_Sequence_Setup(CommPort, *StartDelay, *CaptureTime, *WaitTime, *SampleCount, TOFLASH);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while setting up sequence: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Capture
	printf("Capturing... ");
	if (*TotalLEDCount == 1)
		resp = FeasaCom_Sequence_Capture(CommPort, *Fiber);
	else
		resp = FeasaCom_Sequence_Capture(CommPort, 0);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while performing the Sequence Capture: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Download Intensity results for each fiber
	for (f = 1; f <= *TotalLEDCount; f++) {
		if (((*TotalLEDCount == 1) && (f == *Fiber)) || (*TotalLEDCount > 1)) {
			printf("----- Results for fiber %d -----\n", f);
			resp = FeasaCom_Sequence_ReadxyI(CommPort, f, xValues, yValues, IntensityValues);
			if (resp != 1) {
				FeasaCom_GetError_Description(buffer);
				printf("\nError while downloading results: %s\n", buffer);
				return 0;
			}
			for (i = 0; i < *SampleCount; i++)
				printf("Sample %04d -> %05d %0.4f %0.4f\n", i, IntensityValues[i], xValues[i], yValues[i]);
		}
		printf("\n");
	} //FOR

	//Download lighting pattern for each fiber
	for (f = 1; f <= *TotalLEDCount; f++) {
		if (((*TotalLEDCount == 1) && (f == *Fiber)) || (*TotalLEDCount > 1)) {
			printf("----- Time Pattern for fiber %d -----\n", f);
			resp = FeasaCom_Sequence_GetPattern(CommPort, IntensityValues, &PatternStatusCount, PatternTimes, PatternIntensities);
			if (resp != 1) {
				FeasaCom_GetError_Description(buffer);
				printf("\nError while reading back the pattern: %s\n", buffer);
				return 0;
			}
			for (i = 0; i < PatternStatusCount; i++) {
				if (PatternIntensities[i] == 0)
					printf("LOW (%dms)", PatternTimes[i]);
				else
					printf("HIGH (%dms)", PatternTimes[i]);
				if (i < (PatternStatusCount - 1))printf(", ");
			}
			printf("\n");
		}
	} //FOR

	return 1;
}

int SweepingTest(int CommPort, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount)
{
	char buffer[300];
	int UseDefaultParams = 0;
	int resp;
	int LowTimes[20];
	int HighTimes[20];
	int IntensityValues[20];
	int isOffToOnPattern = 0;
	int i;

	//Check if test parameters were previously initialized
	if (*SampleCount > 0) {
		printf("\nWould you like to use previous parameters?\n");
		printf("   StartDelay=%d ms\n", *StartDelay);
		printf("   TotalLEDCount=%d\n", *TotalLEDCount);
		printf("   CaptureTime=%d ms\n", *CaptureTime);
		printf("   WaitTime=%d ms\n", *WaitTime);
		printf("   SampleCount=%d\n", *SampleCount);
		printf("  Your selection (yes=1, no=0): ");
		scanf("%d", &UseDefaultParams);
		if (UseDefaultParams != 1) UseDefaultParams = 0;
	}

	//If default parameters are not use or does not exists...
	if (UseDefaultParams != 1) {
		//Ask for test parameters
		printf("\nPlease, enter total number of LEDs/fibers to test simultaneously: ");
		scanf("%d", TotalLEDCount);
		if ((*TotalLEDCount<1) || (*TotalLEDCount>20)) *TotalLEDCount = 1;

		printf("\nPlease, enter initial delay before start Capturing [0, 999]ms: ");
		scanf("%d", StartDelay);
		if ((*StartDelay<0) || (*StartDelay>999)) *StartDelay = 0;

		printf("\nPlease, enter Capture time [1, 999]ms: ");
		scanf("%d", CaptureTime);
		if ((*CaptureTime < 1) || (*CaptureTime > 999)) *CaptureTime = 5;

		printf("\nPlease, enter Wait time (time between captures) [0, 999]ms: ");
		scanf("%d", WaitTime);
		if ((*WaitTime < 0) || (*WaitTime > 999)) *WaitTime = 0;

		if (*TotalLEDCount == 1)
			printf("\nPlease, enter number of samples [1, 9999]: ");
		else
			printf("\nPlease, enter number of samples [1, 3500]: ");
		scanf("%d", SampleCount);
		if (*SampleCount < 1) *SampleCount = 10;
		else if ((*SampleCount > 3500) && (*TotalLEDCount > 1)) *SampleCount = 100;
		else if ((*SampleCount > 9999) && (*TotalLEDCount == 1)) *SampleCount = 100;
	}

	printf("\nWould you like to test an Off-to-On pattern? (all LEDs Off initially and switched on sequentially) [no:0, yes:1]: ");
	scanf("%d", &isOffToOnPattern);
	if (isOffToOnPattern < 0) isOffToOnPattern = 0;
	else if (isOffToOnPattern > 1) isOffToOnPattern = 1;

	printf("Setting up sequence... ");
	resp = FeasaCom_Sequence_Setup(CommPort, *StartDelay, *CaptureTime, *WaitTime, *SampleCount, TOFLASH);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while setting up sequence: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	printf("Press any key to start the measurement...");
	i = getch();

	//Capture
	printf("Capturing... ");
	resp = FeasaCom_Sequence_Capture(CommPort, 0);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while performing the Sequence Capture: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Download lighting patterns
	printf("Analysing pattern... ");
	resp = FeasaCom_Sequence_GetSweepingPattern(CommPort, *TotalLEDCount, isOffToOnPattern, LowTimes, HighTimes, IntensityValues);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while downloading results: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Read back intensity pattern
	printf("\n----- LEDs Timing -----\n");
	for (i = 0; i < *TotalLEDCount; i++) {
		if (isOffToOnPattern == 1)
			printf("LED %02d -> LOW: %dms, HIGH: %dms, Int: %d\n", (i + 1), LowTimes[i], HighTimes[i], IntensityValues[i]);
		else
			printf("LED %02d -> HIGH :%dms, LOW: %dms, Int: %d\n", (i + 1), HighTimes[i], LowTimes[i], IntensityValues[i]);
	}
	printf("\n");

	return 1;
}

void FormatDecimal(char * buffer)
{
	int i,l;
	float f=0;
	char decChr, c;

	l= strlen(buffer);

	if ( l>0 ) {
		sscanf("0.253","%f", &f);

		if ( f==(float)(0.253) ) {
			decChr='.'; c = ',';
		} else {
			decChr=','; c = '.';
		}

		for (i=0; i<l; i++) {
			if (buffer[i]==c) buffer[i] = decChr;
		}
	}
}
