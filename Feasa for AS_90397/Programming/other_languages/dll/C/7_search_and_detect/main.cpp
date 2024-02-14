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
#include "feasacom.h"

#define BAUDRATE "AUTO" // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaCom_EnumPorts FeasaCom_EnumPorts;
tFeasaCom_IsConnected FeasaCom_IsConnected;
tFeasaCom_OpenSN FeasaCom_OpenSN;
tFeasaCom_SendSN FeasaCom_SendSN;
tFeasaCom_CloseSN FeasaCom_CloseSN;
tFeasaCom_Detect FeasaCom_Detect;
tFeasaCom_DetectSN FeasaCom_DetectSN;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100];
	char SN[10];
	int i; // auxiliar
	int nDetected = 0; //Used to store number of devices detected
	int Ports[255];
	char * lstSerials[50];

	/* --- DLL ----------------------------------------------------------------*/
	// Load
	printf("Loading DLL...");
	HINSTANCE hFeasaComDLL = LoadLibrary("feasacom.dll");
	if (hFeasaComDLL == NULL) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of DLL functions
	printf("Getting Function addresses...");
	FeasaCom_EnumPorts = (tFeasaCom_EnumPorts)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_EnumPorts");
	FeasaCom_IsConnected = (tFeasaCom_IsConnected)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_IsConnected");
	FeasaCom_OpenSN = (tFeasaCom_OpenSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_OpenSN");
	FeasaCom_SendSN = (tFeasaCom_SendSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendSN");
	FeasaCom_CloseSN = (tFeasaCom_CloseSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseSN");
	FeasaCom_Detect = (tFeasaCom_Detect)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Detect");
	FeasaCom_DetectSN = (tFeasaCom_DetectSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DetectSN");
	if ((FeasaCom_EnumPorts == NULL) || (FeasaCom_IsConnected == NULL) || (FeasaCom_OpenSN == NULL) || (FeasaCom_SendSN == NULL) || (FeasaCom_CloseSN == NULL) || (FeasaCom_Detect == NULL) || (FeasaCom_DetectSN == NULL)) {
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

	//Detect Feasa devices by Port
	nDetected = FeasaCom_Detect(Ports, BAUDRATE);
	if (nDetected==0) {
		printf("No ports detected!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}

	//List all available ports
	printf("List of ports in which a valid Feasa device was found:\n");
	for (i = 0; i < nDetected; i++)
		printf("...Device detected on COM%d\n", Ports[i]);

	//Initialize array of strings
	for (i = 0; i < 50; i++)
		lstSerials[i] = (char *)malloc(10);

	//Detect Feasa devices by Serial Number
	nDetected = FeasaCom_DetectSN(lstSerials, BAUDRATE);
	if (nDetected == 0) {
		printf("No ports detected!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
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
	scanf("%s", SN);
	printf("\n");
	if (strlen(SN) < 4) {
		printf("No valid serial entered!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}

	//Find device's port
	resp = FeasaCom_IsConnected(SN, BAUDRATE);
	if (resp <= 0) {
		printf("Port for device %s was not found\n", SN);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf("Device with SN %s was found on port %d\n", SN, resp);

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!(FeasaCom_OpenSN(SN, BAUDRATE))) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	printf("Capturing...");
	resp = FeasaCom_SendSN(SN, "GETSTATUS", buffer);
	if (resp == -1) {
		printf(" Failed!\n");
		FeasaCom_CloseSN(SN);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	else if (resp == 0) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_CloseSN(SN);
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");
	printf(" |--Response: %s\n", buffer);

	// Close port
	printf("Closing port...");
	if (!FeasaCom_CloseSN(SN)) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaComDLL);
	return 0;
}