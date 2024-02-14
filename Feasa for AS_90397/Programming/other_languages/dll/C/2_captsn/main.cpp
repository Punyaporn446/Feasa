/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture And Read (by Serial Number)
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser using the SN
*  instead of the COM port; then, perform a measurement and
*  download or read back the results
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

#include <windows.h>
#include <stdio.h>
#include "feasacom.h"

#define SERIALNUMBER "E077" // This is the Serial Number of the LED Analyser to which you want to connect
#define BAUDRATE "57600" // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaCom_OpenSN FeasaCom_OpenSN;
tFeasaCom_SendSN FeasaCom_SendSN;
tFeasaCom_CloseSN FeasaCom_CloseSN;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100];
	int i; // auxiliar


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
	FeasaCom_OpenSN=(tFeasaCom_OpenSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_OpenSN");
	FeasaCom_SendSN=(tFeasaCom_SendSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendSN");
	FeasaCom_CloseSN=(tFeasaCom_CloseSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseSN");
	if( (FeasaCom_OpenSN==NULL)||(FeasaCom_SendSN==NULL)||(FeasaCom_CloseSN==NULL) ) {
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

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if ( !(FeasaCom_OpenSN(SERIALNUMBER, BAUDRATE)) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	printf("Capturing...");
	resp = FeasaCom_SendSN(SERIALNUMBER, "CAPTURE", buffer);
	if ( resp==-1 ) {
		printf(" Failed!\n");
		FeasaCom_CloseSN(SERIALNUMBER);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	} else if ( resp==0 ) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_CloseSN(SERIALNUMBER);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");
	printf(" |--Response: %s\n", buffer);


	//Download RGB data
	for (i=1; i<=NUMFIBERS; i++)
	{
		if (i<10) {
			sprintf(command, "GETRGBI0%d", i);
		} else {
			sprintf(command, "GETRGBI%d", i);
		}
		if ( FeasaCom_SendSN(SERIALNUMBER, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_CloseSN(SERIALNUMBER);
			FreeLibrary((HMODULE)hFeasaComDLL);
			return 1;
		}
		printf(" |--Fiber %d: %s\n", i, buffer);
	}


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_CloseSN(SERIALNUMBER) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaComDLL);
	return 0;
}