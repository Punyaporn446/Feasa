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

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include "feasacom.h"

#define COMPORT 6 // This is the comm port where the LED Analyser is connected to
#define BAUDRATE "57600" // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;
tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;

void FormatDecimal(char * buffer);

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	int i; // auxiliar
	double Hue; // to store Hue
	int Saturation; // to store Saturation
	long Intensity; // to store Intensity
	char capture_mode, capture_range, capture_frames; //to store the options selected


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
	FeasaCom_Open=(tFeasaCom_Open)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Open");
	FeasaCom_Send=(tFeasaCom_Send)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send");
	FeasaCom_Close=(tFeasaCom_Close)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Close");
	FeasaCom_SetResponseTimeout=(tFeasaCom_SetResponseTimeout)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SetResponseTimeout");
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)||(FeasaCom_Close==NULL)||(FeasaCom_SetResponseTimeout==NULL) ) {
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

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if ( !FeasaCom_Open(COMPORT, BAUDRATE) ) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");

	//Allow the user to choose the test settings
	printf("\nPlease, select the Capture Mode:\n");
	printf("  0: AUTO\n");
	printf("  1: MANUAL\n");
	printf("  2: PWM: AUTO-RANGE & AUTO-FRAMING\n");
	printf("  3: PWM: MANUAL-RANGE & AUTO-FRAMING\n");
	printf("  4: PWM: AUTO-RANGE & MANUAL-FRAMING\n");
	printf("  5: PWM: MANUAL-RANGE & MANUAL-FRAMING\n");
	printf("  Your selection:");
	scanf("%d", &capture_mode);
	if ( (capture_mode<0)||(capture_mode>5) ) capture_mode = 0;

	if ( (capture_mode==1)||(capture_mode==3)||(capture_mode==5) ) {
		printf("\nPlease, select the Capture Range:\n");
		printf("  1: LOW\n");
		printf("  2: MEDIUM\n");
		printf("  3: HIGH\n");
		printf("  4: SUPER\n");
		printf("  5: ULTRA\n");
		printf("  Your selection:");
		scanf("%d", &capture_range);
		if ( (capture_range<1)||(capture_range>5) ) capture_range = 3;
	}

	if ( (capture_mode==4)||(capture_mode==5) ) {
		printf("\nPlease, select the number of Frames for the averaging (from 1 to 15): ");
		scanf("%d", &capture_frames);
		if ( (capture_frames<1)||(capture_frames>15) ) capture_frames = 5;
	}
	printf("\n\n");

	if (capture_mode==1) {
		sprintf(command, "CAPTURE%d", capture_range);
	} else if (capture_mode==2) {
		strcpy(command, "CAPTUREPWM");
	} else if (capture_mode==3) {
		sprintf(command, "CAPTURE%dPWM", capture_range);
	} else if (capture_mode==4) {
		sprintf(command, "CAPTUREPWM%02d", capture_frames);
	} else if (capture_mode==5) {
		sprintf(command, "CAPTURE%dPWM%02d", capture_range, capture_frames);
	} else {
		strcpy(command, "CAPTURE");
	}

	// Send a command to the LED Analyser
	// Executes a capture
	// The DLL adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	printf("Capturing...");
	resp = FeasaCom_Send(COMPORT, command, buffer);
	if ( resp==-1 ) {
		printf(" Failed!\n");
		FeasaCom_Close(COMPORT);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	} else if ( resp==0 ) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_Close(COMPORT);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");


	//Download HSI data
	printf("\nFiber\t| Hue     \t| Sat\t| Int\n");
	printf("-----------------------------------------\n");
	for (i=1; i<=NUMFIBERS; i++)
	{
		//Format command
		sprintf(command, "GETHSI%.2d", i);
		if ( FeasaCom_Send(COMPORT, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_Close(COMPORT);
			FreeLibrary((HMODULE)hFeasaComDLL);
			return 1;
		}
		// format decimal strings (if needed)
		FormatDecimal(&buffer[0]);
		// parse values
		sscanf(buffer, "%lf %d %d", &Hue, &Saturation, &Intensity);
		printf("  %d\t| %.2f\t| %d\t| %d\n", i, Hue, Saturation, Intensity);
	}
	printf("\n");


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(COMPORT) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaComDLL);
	return 0;
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
