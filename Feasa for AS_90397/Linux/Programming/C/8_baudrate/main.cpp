/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: baudrate
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser picking a
*  known baudrate or detecting it automatically.
*
*  Important Note: it is not possible to communicate to a
*  Led Analyser that does not have axactly the same
*  baudrate used to open the port.
*  Factory default: 57600 baud.
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
#include <time.h>
#include "feasacom.h"

#define DEVPATH "/dev/ttyUSB0" // This is the comm port where the LED Analyser is connected to
#define NUMFIBERS 5 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaComOpen FeasaCom_Open;
tFeasaComSend FeasaCom_Send;
tFeasaComClose FeasaCom_Close;
tFeasaComSetResponseTimeout FeasaCom_SetResponseTimeout;
tFeasaComGetBaudrate FeasaCom_GetBaudrate;

void FormatDecimal(char * buffer);

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	int baudrate; //Used to store the baudrate selected
	int i; // auxiliar
	double Hue; // to store Hue
	int Saturation; // to store Saturation
	long Intensity; // to store Intensity
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
	if(hFeasaCom==NULL) {
		printf(" Failed!\nError: %s\n", dlerror());
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of Library functions
	printf("Getting Function addresses...");
	FeasaCom_Open = (tFeasaComOpen)dlsym(hFeasaCom, "FeasaCom_Open");
	FeasaCom_Send = (tFeasaComSend)dlsym(hFeasaCom, "FeasaCom_Send");
	FeasaCom_Close = (tFeasaComClose)dlsym(hFeasaCom, "FeasaCom_Close");
	FeasaCom_GetBaudrate = (tFeasaComGetBaudrate)dlsym(hFeasaCom, "FeasaCom_GetBaudrate");
	FeasaCom_SetResponseTimeout=(tFeasaComSetResponseTimeout)dlsym(hFeasaCom, "FeasaCom_SetResponseTimeout");
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)|| (FeasaCom_Close == NULL) || (FeasaCom_GetBaudrate == NULL) || (FeasaCom_SetResponseTimeout==NULL) ) {
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
		case 1: baudrate= 9600; break;
		case 2: baudrate= 19200; break;
		case 3: baudrate= 38400; break;
		case 4: baudrate= 57600; break;
		case 5: baudrate= 115200; break;
		case 6: baudrate= 230400; break;
		case 7: baudrate= 460800; break;
		case 8: baudrate= 921600; break;
		default: baudrate=0; break;
	}

	clock_t tIni = clock();

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!FeasaCom_Open(DEVPATH, baudrate)) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");

	if (i == 0)
		printf("Baudrate: %ld\n", FeasaCom_GetBaudrate(DEVPATH));

	// Send a command to the LED Analyser
	// Executes a capture
	// The SO Library adds automatically the couple of characters CR (Carriage return) + LF (Line feed) at the end of each command sent
	// This command will return the response from the Analyser through the third parameter, so be sure to
	// declare it first and to keep enough memory space to store the data returned, according to the expected response.
	printf("Capturing...");
	resp = FeasaCom_Send(DEVPATH, "C", buffer);
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


	//Download HSI data
	printf("\nFiber\t| Hue     \t| Sat\t| Int\n");
	printf("-----------------------------------------\n");
	for (i=1; i<=NUMFIBERS; i++)
	{
		//Format command
		sprintf(command, "GETHSI%.2d", i);
		if ( FeasaCom_Send(DEVPATH, command, buffer)!=1 ) {
			printf("Unable to read values from fiber %d\n", i);
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		}
		// format decimal strings (if needed)
		FormatDecimal(&buffer[0]);
		// parse values
		sscanf(buffer, "%lf %d %ld", &Hue, &Saturation, &Intensity);
		printf("  %d\t| %06.2f\t| %d\t| %ld\n", i, Hue, Saturation, Intensity);
	}
	printf("\n");


	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DEVPATH) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");

	clock_t tEnd = clock();
	double elapsed = (double(tEnd - tIni) / double(CLOCKS_PER_SEC)) * 1000.0;
	printf("Elapsed time: %0.2f ms!\n", elapsed);

	dlclose(hFeasaCom);
	return 0;
}

void FormatDecimal(char * buffer)
{
	int i,l;
	float f=0;
	char decChr, c;

	l= strlen(buffer);

	if ( l>0 ) {
		sscanf("0.253","%f", &f);

		if ( f==(float)(0.253) ) {
			decChr='.'; c = ',';
		} else {
			decChr=','; c = '.';
		}

		for (i=0; i<l; i++) {
			if (buffer[i]==c) buffer[i] = decChr;
		}
	}
}
