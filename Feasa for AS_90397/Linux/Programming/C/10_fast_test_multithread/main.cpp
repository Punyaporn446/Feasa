/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Fast Test (Multi-threaded)
*
*  DESCRIPTION: This example demonstrates how to use the multi-
*  threaded functions provided in the SO Library to set up a
*  fast and efficient communication schema for your application.
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
#include <string>
#include <time.h>
#include "feasacom.h"

#define MAXDEVICES 10 // This is the maximum number of devices

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[1024]; // Used to store the response from the LED Analyser
	int baudrate; //Used to store the baudrate selected
	int i; // auxiliar
	char ** DevPaths = nullptr;
	char ** Serials = nullptr;
	char ** Responses = nullptr;
	int ReturnValues[MAXDEVICES];
	int nDevPaths;

	//Initialize array of strings
	DevPaths = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	Serials = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	Responses = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	for (i = 0; i < MAXDEVICES; i++) {
		DevPaths[i] = (char *)malloc(sizeof(char) * 255);
		Serials[i] = (char *)malloc(sizeof(char) * 100);
		Responses[i] = (char *)malloc(sizeof(char) * 500);
	}
	/*AllocateArrayOfStrings(&lstSerials, 20, 100);
	AllocateArrayOfStrings(&Responses, 20, 100);*/

	/* --- SO Library ----------------------------------------------------------------*/
	// Load
	printf("Loading SO...");
	if (FeasaCom_Load("")==0) {
		printf(" Failed!\n");
		FreeArrayOfStrings(Serials, MAXDEVICES);
		FreeArrayOfStrings(Responses, MAXDEVICES);
		return 1;
	}
	printf(" OK!\n");
	/* ------------------------------------------------------------------------*/


	/*This command enumerates the existing DevPaths to find out
    what are the serial DevPaths available on your computer and
    the devices connected to them. You need to execute this
    command everytime you plug or unplug any Feasa Device,
    while the application is running */
    //FeasaCom_EnumDevPaths();

	//Incrase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
	FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

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
		case 1: baudrate = 9600; break;
		case 2: baudrate = 19200; break;
		case 3: baudrate = 38400; break;
		case 4: baudrate = 57600; break;
		case 5: baudrate = 115200; break;
		case 6: baudrate = 230400; break;
		case 7: baudrate = 460800; break;
		case 8: baudrate = 921600; break;
		default: baudrate = 0; break;
	}

	//Allow the user to enter PDevice Paths
	nDevPaths = 0;
	while (1) {
		buffer[0] = '\0';
		printf("\n[%d DevPaths added]\n", nDevPaths);
		printf("Please, enter port number or type '0' to finish:");
		scanf("%s", buffer);
		if (strlen(buffer)>=5) {
			 strcpy(DevPaths[nDevPaths++], buffer);
		}
		else
			break;
	}
	if (nDevPaths == 0) {
		printf(" No serials added!\n");
		FeasaCom_UnLoad();
		FreeArrayOfStrings(Serials, MAXDEVICES);
		FreeArrayOfStrings(Responses, MAXDEVICES);
		return 1;
	}

	clock_t tIni = clock();

	if (FeasaCom_Open_Multi(ReturnValues, (const char **)DevPaths, nDevPaths, baudrate) == 1)
	{
		//No error

		//Retrieve Serial numbers connected
		for (int i = 0; i < nDevPaths; i++)
		{
			resp = FeasaCom_GetSNByPort(buffer, DevPaths[i]);
			if (resp == 1)
				strcpy(Serials[i], buffer);
			else
				*Serials[i] = '\0';
		}

		//Send command to All Analysers connected
		resp = FeasaCom_SendToAll(ReturnValues, "CAPTURE", Responses);
		if (resp != 1)
		{
			for (int i = 0; i < nDevPaths; i++)
				if (ReturnValues[i] == -1)
				{
					printf("Unable to send the command to %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, (const char **)DevPaths, nDevPaths);
					FeasaCom_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
				else if (ReturnValues[i] == 0)
				{
					printf("Timeout or Syntax error detected in %s", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, (const char **)DevPaths, nDevPaths);
					FeasaCom_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
		}

		//Send command to All Analysers connected
		resp = FeasaCom_SendToAll(ReturnValues, "GETHSIALL", Responses);
		if (resp != 1)
		{
			for (int i = 0; i < nDevPaths; i++)
				if (ReturnValues[i] == -1)
				{
					printf("Unable to send the command to %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, (const char **)DevPaths, nDevPaths);
					FeasaCom_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
				else if (ReturnValues[i] == 0)
				{
					printf("Timeout or Syntax error detected in %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, (const char **)DevPaths, nDevPaths);
					FeasaCom_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
		}

		//Extract response lines and parse responses
		for (int i = 0; i < nDevPaths; i++)
		{
			//Declare numerical arrays for responses with 20 elements (maximum number of channels)
			printf("%s\n", Responses[i]);
		}

		//Close the port
		FeasaCom_Close_Multi(ReturnValues, (const char **)DevPaths, nDevPaths);
	}
	else
	{
		//Error: unable to open the selected port
		printf("Unable to open all DevPaths");
	}

	clock_t tEnd = clock();
	double elapsed = (double(tEnd - tIni) / double(CLOCKS_PER_SEC)) * 1000.0;
	printf("Elapsed time: %0.2f ms!\n", elapsed);
	FeasaCom_UnLoad();
	
	FreeArrayOfStrings(Serials, MAXDEVICES);
	FreeArrayOfStrings(Responses, MAXDEVICES);

	return 0;
}
