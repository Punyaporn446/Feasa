/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: tools_lib
*
*  DESCRIPTION: This example demonstrates how to use the
*  library feasa_tools64.dll or its 32-bit equivalent
*  feasa_tools.dll, to extract or parse the strings returned
*  by the Feasa LED Analyser responses and convert them
*  into usable numerical values.
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
#include <string.h>
#include "feasacom.h"
#include "feasa_tools.h"

#define COMPORT 6 // This is the comm port where the LED Analyser is connected to
#define BAUDRATE "57600" // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;
tFeasaCom_ParseRGBI Feasa_Parse_RGBI;
tFeasaCom_ParseHSI Feasa_Parse_HSI;
tFeasaCom_ParseInt16 Feasa_Parse_Int16;

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	int i; // auxiliar
	unsigned char Red, Green, Blue; //Used to store the RGB data
	float Hue; // to store Hue
	int Saturation; // to store Saturation
	int Intensity; // to store Intensity


	/* --- LED Analyser DLL ----------------------------------------------------------------*/
	// Load
	HINSTANCE hFeasaDLL=LoadLibrary("feasacom.dll");
	if(hFeasaDLL==NULL) {
		return 1;
	}
	// Get Addresses of DLL functions
	FeasaCom_Open=(tFeasaCom_Open)GetProcAddress((HMODULE)hFeasaDLL, "FeasaCom_Open");
	FeasaCom_Send=(tFeasaCom_Send)GetProcAddress((HMODULE)hFeasaDLL, "FeasaCom_Send");
	FeasaCom_Close=(tFeasaCom_Close)GetProcAddress((HMODULE)hFeasaDLL, "FeasaCom_Close");
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)||(FeasaCom_Close==NULL) ) {
		FreeLibrary((HMODULE)hFeasaDLL);
		return 1;
	}
	/* ------------------------------------------------------------------------*/

	/* --- Lib Tools DLL ----------------------------------------------------------------*/
	// Load
	HINSTANCE hFeasaToolsDLL=LoadLibrary("feasa_tools.dll");
	if(hFeasaToolsDLL==NULL) {
		return 1;
	}
	// Get Addresses of DLL functions
	Feasa_Parse_RGBI=(tFeasaCom_ParseRGBI)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_RGBI");
	Feasa_Parse_HSI=(tFeasaCom_ParseHSI)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_HSI");
	Feasa_Parse_Int16=(tFeasaCom_ParseInt16)GetProcAddress((HMODULE)hFeasaToolsDLL, "Feasa_Parse_Int16");
	if( (Feasa_Parse_RGBI==NULL)||(Feasa_Parse_HSI==NULL)||(Feasa_Parse_Int16==NULL) ) {
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		return 1;
	}
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
		FreeLibrary((HMODULE)hFeasaDLL);
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		return 1;
	}
	printf(" OK!\n");


	// Send a command to the LED Analyser
	// Executes a capture
	// The DLL adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	printf("Capturing...");
	resp = FeasaCom_Send(COMPORT, "CAPTURE", buffer);
	if ( resp==-1 ) {
		printf(" Failed!\n");
		FeasaCom_Close(COMPORT);
		FreeLibrary((HMODULE)hFeasaDLL);
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		return 1;
	} else if ( resp==0 ) {
		printf(" Syntax error or Timeout detected!\n");
		FeasaCom_Close(COMPORT);
		FreeLibrary((HMODULE)hFeasaDLL);
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		return 1;
	}
	printf(" OK!\n");


	//Download HSI data
	printf("\nFiber\t| Hue     \t| Sat\t| Int\t| Red\t| Green\t| Blue\n");
	printf("-----------------------------------------------------------------\n");
	for (i=1; i<=NUMFIBERS; i++)
	{
		//Format command
		sprintf(command, "GETHSI%.2d", i);
		if ( FeasaCom_Send(COMPORT, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_Close(COMPORT);
			FreeLibrary((HMODULE)hFeasaDLL);
			FreeLibrary((HMODULE)hFeasaToolsDLL);
			return 1;
		}
		//Parse values
		Feasa_Parse_HSI(buffer, &Hue, &Saturation, &Intensity);

		//Format command
		sprintf(command, "GETRGBI%.2d", i);
		if ( FeasaCom_Send(COMPORT, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_Close(COMPORT);
			FreeLibrary((HMODULE)hFeasaDLL);
			FreeLibrary((HMODULE)hFeasaToolsDLL);
			return 1;
		}
		//Parse values
		Feasa_Parse_RGBI(buffer, &Red, &Green, &Blue, &Intensity);
		printf("  %d\t| %.2f\t| %d\t| %d\t| %d\t| %d\t| %d\n", i, Hue, Saturation, Intensity, Red, Green, Blue);
	}
	printf("\n");


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(COMPORT) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaDLL);
		FreeLibrary((HMODULE)hFeasaToolsDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaDLL);
	FreeLibrary((HMODULE)hFeasaToolsDLL);
	return 0;
}

