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
*  instead of the Device Path; then, perform a measurement and
*  download or read back the results
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
#include <stdio.h>
#include <dlfcn.h>
#include "feasacom.h"

#define SERIALNUMBER "E077" // This is the Serial Number of the LED Analyser to which you want to connect
#define BAUDRATE 57600 // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaComOpenSN FeasaCom_OpenSN;
tFeasaComSendSN FeasaCom_SendSN;
tFeasaComCloseSN FeasaCom_CloseSN;

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
	FeasaCom_OpenSN=(tFeasaComOpenSN)dlsym(hFeasaCom, "FeasaCom_OpenSN");
	FeasaCom_SendSN=(tFeasaComSendSN)dlsym(hFeasaCom, "FeasaCom_SendSN");
	FeasaCom_CloseSN=(tFeasaComCloseSN)dlsym(hFeasaCom, "FeasaCom_CloseSN");
	if( (FeasaCom_OpenSN==NULL)||(FeasaCom_SendSN==NULL)||(FeasaCom_CloseSN==NULL) ) {
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
	if ( !(FeasaCom_OpenSN(SERIALNUMBER, BAUDRATE)) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	printf("Capturing...");
	resp = FeasaCom_SendSN(SERIALNUMBER, "CAPTURE", buffer);
	if ( resp==-1 ) {
		printf(" Failed!\n");
		FeasaCom_CloseSN(SERIALNUMBER);
		dlclose(hFeasaCom);
		return 1;
	} else if ( resp==0 ) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_CloseSN(SERIALNUMBER);
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
		if ( FeasaCom_SendSN(SERIALNUMBER, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_CloseSN(SERIALNUMBER);
			dlclose(hFeasaCom);
			return 1;
		}
		printf(" |--Fiber %d: %s\n", i, buffer);
	}


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_CloseSN(SERIALNUMBER) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");

	dlclose(hFeasaCom);
	return 0;
}
