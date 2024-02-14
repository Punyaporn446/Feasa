/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture and Read
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser, perform a
*  measurement and download or read back the results
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
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaComOpen FeasaCom_Open;
tFeasaComSend FeasaCom_Send;
tFeasaComClose FeasaCom_Close;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100];
	int i; // auxiliar
	void * hFeasaCom;


	/* --- SO Library ----------------------------------------------------------------*/
	// Load
	printf("Loading SO...");
	hFeasaCom = dlopen("libfeasacom.so", RTLD_LAZY);
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
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	// Executes a capture
	// The Library adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	printf("Capturing...");
	resp = FeasaCom_Send(DEVPATH, "CAPTURE", buffer);
	if ( resp==-1 ) {
		printf(" Failed!\n");
		FeasaCom_Close(DEVPATH);
		dlclose(hFeasaCom);
		return 1;
	} else if ( resp==0 ) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_Close(DEVPATH);
		dlclose(hFeasaCom);
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
		if ( FeasaCom_Send(DEVPATH, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		}
		printf(" |--Fiber %d: %s\n", i, buffer);
	}



	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DEVPATH) ) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");

	dlclose(hFeasaCom);

	return 0;
}
