/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Fast Test (Multi-threaded)
*
*  DESCRIPTION: This example demonstrates how to use the multi-
*  threaded functions provided in the DLL to set up a fast and
*  efficient communication schema for your application.
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
#include <conio.h>
#include <string.h>
#include <string>
#include <time.h>
#include "feasacom.h"
#include "feasatools.h"

#define MAXDEVICES 10 // This is the maximum number of devices

void FreeArrayOfStrings(char ** mArrayOfStrings, int Size)
{
	int i;
	for (i = 0; i < Size; i++)
		free(mArrayOfStrings[i]);
	free(mArrayOfStrings);
}

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	char baudrate[20]; //Used to store the baudrate selected
	int i; // auxiliar
	int Ports[MAXDEVICES];
	char ** Serials = nullptr;
	char ** Responses = nullptr;
	int ReturnValues[MAXDEVICES];
	int nPorts;
	float HueValues[20];
	int SaturationValues[20];
	int IntensityValues[20];

	//Initialize array of strings
	Serials = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	Responses = (char **)malloc(size_t(sizeof(char *) * MAXDEVICES));
	for (i = 0; i < MAXDEVICES; i++) {
		Serials[i] = (char *)malloc(sizeof(char) * 100 + 1);
		Responses[i] = (char *)malloc(sizeof(char) * 500 + 1);
	}

	/* --- DLL ----------------------------------------------------------------*/
	// Load
	printf("Loading DLL...");
	if (FeasaCom_Load(0, "")==0) {
		printf(" Failed!\n");
		FreeArrayOfStrings(Serials, MAXDEVICES);
		FreeArrayOfStrings(Responses, MAXDEVICES);
		return 1;
	}
	printf(" OK!\n");
	printf("Loading Toools DLL...");
	if (FeasaTools_Load(0, "") == 0) {
		printf(" Failed!\n");
		FeasaCom_UnLoad();
		FreeArrayOfStrings(Serials, MAXDEVICES);
		FreeArrayOfStrings(Responses, MAXDEVICES);
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
		case 1: strcpy(baudrate, "9600"); break;
		case 2: strcpy(baudrate, "19200"); break;
		case 3: strcpy(baudrate, "38400"); break;
		case 4: strcpy(baudrate, "57600"); break;
		case 5: strcpy(baudrate, "115200"); break;
		case 6: strcpy(baudrate, "230400"); break;
		case 7: strcpy(baudrate, "460800"); break;
		case 8: strcpy(baudrate, "921600"); break;
		default: strcpy(baudrate, "AUTO"); break;
	}

	//Allow the user to enter Port Numbers
	nPorts = 0;
	while (1) {
		buffer[0] = '\0';
		printf("\n[%d Ports added]\n", nPorts);
		printf("Please, enter port number or type '0' to finish:");
		scanf("%d", &i);
		if (i > 0) {
			Ports[nPorts++] = i;
		}
		else
			break;
	}
	if (nPorts == 0) {
		printf(" No serials added!\n");
		FeasaCom_UnLoad();
		FeasaTools_UnLoad();
		FreeArrayOfStrings(Serials, MAXDEVICES);
		FreeArrayOfStrings(Responses, MAXDEVICES);
		return 1;
	}

	clock_t tIni = clock();

	if (FeasaCom_Open_Multi(ReturnValues, Ports, nPorts, baudrate) == 1)
	{
		//No error

		//Retrieve Serial numbers connected
		for (int i = 0; i < nPorts; i++)
		{
			resp = FeasaCom_GetSNByPort(buffer, Ports[i]);
			if (resp == 1)
				strcpy(Serials[i], buffer);
			else
				Serials[i] = '\0';
		}

		//Send command to All Analysers connected
		resp = FeasaCom_SendToAll(ReturnValues, "CAPTURE", Responses);
		if (resp != 1)
		{
			for (int i = 0; i < nPorts; i++)
				if (ReturnValues[i] == -1)
				{
					printf("Unable to send the command to %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
					FeasaCom_UnLoad();
					FeasaTools_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
				else if (ReturnValues[i] == 0)
				{
					printf("Timeout or Syntax error detected in %s", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
					FeasaCom_UnLoad();
					FeasaTools_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
		}

		//Send command to All Analysers connected
		resp = FeasaCom_SendToAll(ReturnValues, "GETHSIALL", Responses);
		if (resp != 1)
		{
			for (int i = 0; i < nPorts; i++)
				if (ReturnValues[i] == -1)
				{
					printf("Unable to send the command to %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
					FeasaCom_UnLoad();
					FeasaTools_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
				else if (ReturnValues[i] == 0)
				{
					printf("Timeout or Syntax error detected in %s!", Serials[i]);
					FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
					FeasaCom_UnLoad();
					FeasaTools_UnLoad();
					FreeArrayOfStrings(Serials, MAXDEVICES);
					FreeArrayOfStrings(Responses, MAXDEVICES);
					return 1;
				}
		}

		//Extract response lines and parse responses
		for (int i = 0; i < nPorts; i++)
		{
			int nLines = Feasa_Parse_HSI_All(Responses[i], HueValues, SaturationValues, IntensityValues);
			if (nLines > 0)
			{
				for (int f = 0; f < nLines; f++)
					printf("%s\t%02d\t%03.2f\t%03d\t%05d\n", Serials[i], f + 1, HueValues[f], SaturationValues[f], IntensityValues[f]);
			}
		}

		//Close the port
		FeasaCom_Close_Multi(ReturnValues, Ports, nPorts);
	}
	else
	{
		//Error: unable to open the selected port
		printf("Unable to open all ports");
	}

	clock_t tEnd = clock();
	double elapsed = (double(tEnd - tIni) / double(CLOCKS_PER_SEC)) * 1000.0;
	printf("Elapsed time: %0.2f ms!\n", elapsed);
	FeasaCom_UnLoad();
	FeasaTools_UnLoad();

	FreeArrayOfStrings(Serials, MAXDEVICES);
	FreeArrayOfStrings(Responses, MAXDEVICES);

	return 0;
}
