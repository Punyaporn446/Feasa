/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Search & Detect
*
*  DESCRIPTION: This example demonstrates how to list
*  all available Feasa devices, and also to locate
*  the port number of a connected Device based on its serial
*  number.
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

#define BAUDRATE 0 // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser
#define MAXDEVICES 50

// Declare pointers to call functions
tFeasaComEnumPorts FeasaCom_EnumPorts;
tFeasaComIsConnected FeasaCom_IsConnected;
tFeasaComOpenSN FeasaCom_OpenSN;
tFeasaComSendSN FeasaCom_SendSN;
tFeasaComCloseSN FeasaCom_CloseSN;
tFeasaComDetect FeasaCom_Detect;
tFeasaComDetectSN FeasaCom_DetectSN;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char SN[10];
	int i; // auxiliar
	int nDetected = 0; //Used to store number of devices detected
	char ** DevPaths;
	char * lstSerials[50];
	void * hFeasaCom;

	/* --- SO Library ----------------------------------------------------------------*/
	// Load
	printf("Loading SO...");
	#ifdef __LP64__
	#pragma message("Compiling to 64bit...")
	hFeasaCom = dlopen("libfeasacom.x86_64.so", RTLD_LAZY);
	#else
	#pragma message("Compiling to 32bit...")
	hFeasaCom = dlopen("libfeasacom.so", RTLD_GLOBAL | RTLD_LAZY);
	#endif // __LP64__
	if (hFeasaCom == NULL) {
		printf(" Failed!\nError: %s\n", dlerror());
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of Library functions
	printf("Getting Function addresses...");
	FeasaCom_EnumPorts = (tFeasaComEnumPorts)dlsym(hFeasaCom, "FeasaCom_EnumPorts");
	FeasaCom_IsConnected = (tFeasaComIsConnected)dlsym(hFeasaCom, "FeasaCom_IsConnected");
	FeasaCom_OpenSN = (tFeasaComOpenSN)dlsym(hFeasaCom, "FeasaCom_OpenSN");
	FeasaCom_SendSN = (tFeasaComSendSN)dlsym(hFeasaCom, "FeasaCom_SendSN");
	FeasaCom_CloseSN = (tFeasaComCloseSN)dlsym(hFeasaCom, "FeasaCom_CloseSN");
	FeasaCom_Detect = (tFeasaComDetect)dlsym(hFeasaCom, "FeasaCom_Detect");
	FeasaCom_DetectSN = (tFeasaComDetectSN)dlsym(hFeasaCom, "FeasaCom_DetectSN");
	if ((FeasaCom_EnumPorts == NULL) || (FeasaCom_IsConnected == NULL) || (FeasaCom_OpenSN == NULL) || (FeasaCom_SendSN == NULL) || (FeasaCom_CloseSN == NULL) || (FeasaCom_Detect == NULL) || (FeasaCom_DetectSN == NULL)) {
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

	//Initialize array of strings
	DevPaths = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	for (i = 0; i < MAXDEVICES; i++) {
		DevPaths[i] = (char *)malloc(sizeof(char) * 255);
	}

	//Detect Feasa devices by Port
	nDetected = FeasaCom_Detect(DevPaths, BAUDRATE);
	if (nDetected==0) {
		printf("No ports detected!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf("%d Feasa devices detected.\n", nDetected);

	//List all available ports
	printf("List of ports in which a valid Feasa device was found:\n");
	for (i = 0; i < nDetected; i++)
		printf("...Device detected on %s\n", DevPaths[i]);

	//Initialize array of strings
	for (i = 0; i < 50; i++)
		lstSerials[i] = (char *)malloc(10);

	//Detect Feasa devices by Serial Number
	nDetected = FeasaCom_DetectSN(lstSerials, BAUDRATE);
	if (nDetected == 0) {
		printf("No ports detected!\n");
		dlclose(hFeasaCom);
		for (i = 0; i < 50; i++) free(lstSerials[i]);
		return 1;
	}

	//List all available ports
	printf("List of Serial Numbers of Feasa devices found:\n");
	for (i = 0; i < nDetected; i++)
		printf("...SN:%s detected\n", lstSerials[i]);

	//Free list of serials
	for (i = 0; i < 50; i++) free(lstSerials[i]);

	//Retrieve Serial Number to find
	printf("Please, enter Device's Serial Number to find: ");
	resp = scanf("%s", SN);
	printf("\n");
	if (strlen(SN) < 4) {
		printf("No valid serial entered!\n");
		dlclose(hFeasaCom);
		return 1;
	}

	//Find device's port
	resp = FeasaCom_IsConnected(buffer, SN, BAUDRATE);
	if (resp <= 0) {
		printf("Port for device %s was not found\n", SN);
		dlclose(hFeasaCom);
		return 1;
	}
	printf("Device with SN %s was found on port %s\n", SN, buffer);

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!(FeasaCom_OpenSN(SN, BAUDRATE))) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	printf("Capturing...");
	resp = FeasaCom_SendSN(SN, "GETSTATUS", buffer);
	if (resp == -1) {
		printf(" Failed!\n");
		FeasaCom_CloseSN(SN);
		dlclose(hFeasaCom);
		return 1;
	}
	else if (resp == 0) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_CloseSN(SN);
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");
	printf(" |--Response: %s\n", buffer);

	// Close port
	printf("Closing port...");
	if (!FeasaCom_CloseSN(SN)) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");

	dlclose(hFeasaCom);
	return 0;
}
