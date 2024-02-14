/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence
*
*  DESCRIPTION: This example demonstrates how to use Sequence
*  functions provided in the SO Library to test a blinking LED and
*  a sweeping light pattern made by several LEDs so that the
*  light pattern could be tracked.
*
*  This example uses a dynamic library to access to the
*  functions related to the LED Analyser. This library is read
*  in runtime, so you have to be sure that the file
*  libfeasacom_x86_64.so has been copied to the /usr/lib/
*  directory or equivalent, moreover,  some compillers/IDE
*  allow to reference the SO library from the same location
*  of the binary/script or alternative locations using absolute
*  or relative paths.
*
*  Note: there are 32 and 64-bit versions of the SO Library, so
*  one or the other has to be used depending on the compiler/IDE
*  platform or binary target platform.
*
***************************************************************/

#include <stdlib.h>
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include "feasacom.h"

#define TOFLASH 0 //Decides whether adjustments are stored in flash or RAM

void FormatDecimal(char * buffer);
int FindParamaters(const char * DevPath, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber);
int SequenceTest(const char * DevPath, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber);
int SweepingTest(const char * DevPath, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount);

int main(int argc, char* argv[])
{
	// Variables
	char buffer[100]; // Used to store the composed command
	char DevPath[255]; //To store device path
	int Baudrate = 0;
	int i, n; // auxiliar
	int CaptureTime = 0; // to store Capture time
	int WaitTime = 0; // to store Time between captures
	int SampleCount = 0; // to store Number of samples
	int TotalLEDCount = 0; // to stores Total numbers of LEDs to be tested simultaneously
	int Fiber = 0; //To store Fiber/channel to be used for sequence (0 for all)
	int StartDelay = 0; //To store initial delay before start capturing the sequence

	/* --- SO Library ----------------------------------------------------------------*/
	// Load
	printf("Loading SO...");
	if (FeasaCom_Load("")==0) {
		printf(" Failed!\n");
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

	//Incrase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
	FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

	//Allow the user to choose the test settings
	n = 0;
	printf("\nPlease, choose Port:\n");
	for (i = 0; i<20; i++) {
        sprintf(buffer, "/dev/ttyUSB%d", i);
		if (FeasaCom_IsPortAvailable(buffer) == 1) {
			printf("  %d: %s\n", i, buffer);
			n++;
        }
    }
    if (n==0) {
		printf(" No ports have been detected!\n");
		return 1;
    }
	printf("  Your selection:");
	scanf("%d", &i);
	if ((i<0) || (i>255)) i = 0;
	sprintf(DevPath, "/dev/ttyUSB%d", i);

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
		case 1: Baudrate = 9600; break;
		case 2: Baudrate = 19200; break;
		case 3: Baudrate = 38400; break;
		case 4: Baudrate = 57600; break;
		case 5: Baudrate = 115200; break;
		case 6: Baudrate = 230400; break;
		case 7: Baudrate = 460800; break;
		case 8: Baudrate = 921600; break;
		default: Baudrate = 0; break;
	}

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!FeasaCom_Open(DevPath, Baudrate)) {
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
			FindParamaters(DevPath, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount, &Fiber);
		}
		else if (i == 2) {
			SequenceTest(DevPath, &StartDelay, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount, &Fiber);
		}
		else if (i == 3) {
			SweepingTest(DevPath, &StartDelay, &CaptureTime, &WaitTime, &SampleCount, &TotalLEDCount);
		}
		else {
			break;
		}

	} //WHILE

	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DevPath) ) {
		printf(" Failed!\n");
		FeasaCom_UnLoad();
		return 1;
	}
	printf(" OK!\n");

	FeasaCom_UnLoad();
	return 0;
}

int FindParamaters(const char * DevPath, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber)
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

	resp = FeasaCom_Sequence_FindTestSettings(DevPath, *TotalLEDCount, *Fiber, SignalSpeed, BlinkingSpeed, CycleCount, TimeResolutionIsImportant, CaptureTime, WaitTime, SampleCount);
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

int SequenceTest(const char * DevPath, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount, int * Fiber)
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
	resp = FeasaCom_Sequence_Setup(DevPath, *StartDelay, *CaptureTime, *WaitTime, *SampleCount, TOFLASH);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while setting up sequence: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Capture
	printf("Capturing... ");
	if (*TotalLEDCount == 1)
		resp = FeasaCom_Sequence_Capture(DevPath, *Fiber);
	else
		resp = FeasaCom_Sequence_Capture(DevPath, 0);
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
			resp = FeasaCom_Sequence_ReadxyI(DevPath, f, xValues, yValues, IntensityValues);
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
			resp = FeasaCom_Sequence_GetPattern(DevPath, IntensityValues, &PatternStatusCount, PatternTimes, PatternIntensities);
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

int SweepingTest(const char * DevPath, int * StartDelay, int * CaptureTime, int * WaitTime, int * SampleCount, int * TotalLEDCount)
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
	resp = FeasaCom_Sequence_Setup(DevPath, *StartDelay, *CaptureTime, *WaitTime, *SampleCount, TOFLASH);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while setting up sequence: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	printf("Press any key to start the measurement...");
	i = getchar();

	//Capture
	printf("Capturing... ");
	resp = FeasaCom_Sequence_Capture(DevPath, 0);
	if (resp != 1) {
		FeasaCom_GetError_Description(buffer);
		printf("\nError while performing the Sequence Capture: %s\n", buffer);
		return 0;
	}
	printf("OK!\n");

	//Download lighting patterns
	printf("Analysing pattern... ");
	resp = FeasaCom_Sequence_GetSweepingPattern(DevPath, *TotalLEDCount, isOffToOnPattern, LowTimes, HighTimes, IntensityValues);
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

