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
*  a method to load the SO library and to call the functions
*  provided.
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
**************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include "feasacom.h"

#define DEVPATH "/dev/ttyUSB0" // This is the comm port where the LED Analyser is connected to
#define BAUDRATE 57600 // This is the baudrate configured in the LED Analyser

// Declare pointers to call functions
tFeasaComOpen FeasaCom_Open;
tFeasaComSend FeasaCom_Send;
tFeasaComClose FeasaCom_Close;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	void * hFeasaCom;

	/* --- SO Library ----------------------------------------------------------------*/
	// Load
	printf("Loading SO...");
	hFeasaCom = dlopen("libfeasacom.x86_64.so", RTLD_LAZY);
	if(hFeasaCom==NULL) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of Library functions
	printf("Getting Function addresses...");
	FeasaCom_Open=(tFeasaComOpen)dlsym(hFeasaCom, "FeasaCom_Open");
	FeasaCom_Send=(tFeasaComSend)dlsym(hFeasaCom, "FeasaCom_Send");
	FeasaCom_Close=(tFeasaComClose)dlsym(hFeasaCom, "FeasaCom_Close");
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)||(FeasaCom_Close==NULL) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
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
	if ( !FeasaCom_Open(DEVPATH, BAUDRATE) ) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	// Ask for the Serial Number
	// The Library adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	// Response --> 1:OK, 0:Timeout or Syntax Error, -1: Unknown error
	printf("Retrieving serial...");
	resp = FeasaCom_Send(DEVPATH, "GETSERIAL", buffer);
	if ( resp!=1 ) {
		printf(" Failed!\n");
		FeasaCom_Close(DEVPATH);
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");
	printf(" |--Response: %s\n", buffer);


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DEVPATH) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");


	//Unload SO
	dlclose(hFeasaCom);

	return 0;
}
