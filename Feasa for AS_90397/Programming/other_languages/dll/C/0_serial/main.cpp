/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: getserial
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser. It also shows
*  a method to load the DLL library and to call the functions
*  provided.
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

#define COMPORT 6 // This is the comm port where the LED Analyser is connected to
#define BAUDRATE "57600" // This is the baudrate configured in the LED Analyser

// Declare pointers to call functions
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser

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
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)||(FeasaCom_Close==NULL) ) {
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
	if ( !FeasaCom_Open(COMPORT, BAUDRATE) ) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	// Ask for the Serial Number
	// The DLL adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	// Response --> 1:OK, 0:Timeout or Syntax Error, -1: Unknown error
	printf("Retrieving serial...");
	resp = FeasaCom_Send(COMPORT, "GETSERIAL", buffer);
	if ( resp!=1 ) {
		printf(" Failed!\n");
		FeasaCom_Close(COMPORT);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");
	printf(" |--Response: %s\n", buffer);


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(COMPORT) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");


	//Unload DLL
	FreeLibrary((HMODULE)hFeasaComDLL);

	return 0;
}