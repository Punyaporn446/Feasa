/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain (Bus capture)
*
*  DESCRIPTION: This example demonstrates how to perform
*  a capture for all Daisy-chained analysers, through the
*  SO Library functions and then retrieve the HSI values for
*  the fiber requested.
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
#include <string.h>
#include <dlfcn.h>
#include "feasacom.h"

#define DEVPATH "/dev/ttyUSB0" // This is the comm port where the LED Analyser is connected to
#define BAUDRATE 57600 // This is the baudrate configured in the LED Analyser
#define NUMFIBERS 10 // Number of fibers/channels of the LED Analyser

// Declare pointers to call functions
tFeasaComOpen FeasaCom_Open;
tFeasaComSend FeasaCom_Send;
tFeasaComClose FeasaCom_Close;
tFeasaComDaisyChainAdd FeasaCom_DaisyChain_Add;
tFeasaComDaisyChainCapture FeasaCom_DaisyChain_Capture;
tFeasaComGetErrorDescription FeasaCom_GetError_Description;

void FormatDecimal(char * buffer);

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char buffer[512]; // Used to store the response from the LED Analyser
	char command[100]; // Used to store the composed command
	int i; // auxiliar
	int FibNum; //to store fiber number to read
	int capture_mode, capture_range; //to store the options selected
	char BusAnalysers[10][20]; //to Store Serial Numbres of Analysers attached to the Daisy-Chain bus
	int BusAnalysers_Count = 0;
	char SN_main[10];
	void * hFeasaCom;

	/* --- Feasa Library ----------------------------------------------------------------*/
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
	FeasaCom_DaisyChain_Add=(tFeasaComDaisyChainAdd)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Add");
	FeasaCom_DaisyChain_Capture=(tFeasaComDaisyChainCapture)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Capture");
	if( (FeasaCom_Open==NULL)||(FeasaCom_Send==NULL)||(FeasaCom_Close==NULL)||(FeasaCom_DaisyChain_Add==NULL)||(FeasaCom_DaisyChain_Capture==NULL) ) {
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

	//Allow the user to choose the test settings
	printf("\nPlease, select the Capture Mode:\n");
	printf("  0: AUTO\n");
	printf("  1: MANUAL\n");
	printf("  Your selection:");
	scanf("%d", &capture_mode);
	if ( (capture_mode<0)||(capture_mode>5) ) capture_mode = 0;

	//Ask for Capture Range
	if ( (capture_mode==1)||(capture_mode==3)||(capture_mode==5) ) {
		printf("\nPlease, select the Capture Range:\n");
		printf("  1: LOW\n");
		printf("  2: MEDIUM\n");
		printf("  3: HIGH\n");
		printf("  4: SUPER\n");
		printf("  5: ULTRA\n");
		printf("  Your selection:");
		scanf("%d", &capture_range);
		if ( (capture_range<1)||(capture_range>5) ) capture_range = 3;
	}

	//Ask for fiber number
	FibNum = 1;
	printf("\nPlease, enter the number of the fiber/channel to read (1-20): ");
	scanf("%d", &FibNum);
	if ( (FibNum<1)||(FibNum>20) ) FibNum = 1;

	//Ask for Serial Numbers of Analysers attached to the Daisy Chain Bus
	do {
		printf("\nPlease, enter the Serial Number of a Analyser attached to the Daisy-Chain Bus (leave empty to finish adding Analysers): ");
		gets(buffer);
		i = strlen(buffer);
		if ( i==4 ) {
			strcpy(BusAnalysers[BusAnalysers_Count], buffer);
			BusAnalysers_Count++;
		} else if (i>0) {
			printf(" -BAD SERIAL ENTERED!-");
		}
	} while( (strlen(buffer)>0)||(BusAnalysers_Count==0) );
	printf("\n\n");

	if (capture_mode==0) {
		capture_range = 0;
	}

	//Add Analysers to the bus structure
	printf("Adding Analysers to the BUS structure...");
	for (i=0; i<BusAnalysers_Count; i++) {
		resp = FeasaCom_DaisyChain_Add(DEVPATH, BusAnalysers[i]);
		if ( resp==0 ) {
			printf("Unable to add Analyser %s to the BUS\n", BusAnalysers[i]);
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		}
	}
	printf(" OK!\n");

	// Performs a capture on the LED Analyser
	printf("Capturing...");
	resp = FeasaCom_DaisyChain_Capture(DEVPATH, 0, capture_range, 0);
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

	// Free BUS
	printf("Free BUS...");
	resp = FeasaCom_Send(DEVPATH, "BUSFREE", buffer);
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

	// Retrieve Serial Number of Main/Master Analyser
	printf("Reading Serial...");
	resp = FeasaCom_Send(DEVPATH, "GETSERIAL", buffer);
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
	strcpy(SN_main, buffer);

	// Retrieve measured data from the Main Analyser
	printf("Downloading measured data Data...\n");
	sprintf(command, "GETHSI%.2d", FibNum);
	resp = FeasaCom_Send(DEVPATH, command, buffer);
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
	printf("Fib %02d (%s): %s\n", FibNum, SN_main, buffer);


	// Retrieve measured data from Analysers attached to the BUS
	for (i=0; i<BusAnalysers_Count; i++) {
		// Set the BUs owner
		sprintf(command, "BUSGET%s", BusAnalysers[i]);
		resp = FeasaCom_Send(DEVPATH, command, buffer);
		if ( resp==-1 ) {
			printf("Unable to Free the BUS\n");
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		} else if ( resp==0 ) {
			printf(" Syntax error or Timeout detected!\n");
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		}

		// Retrieve measured data from the Main Analyser
		sprintf(command, "GETHSI%.2d", FibNum);
		resp = FeasaCom_Send(DEVPATH, command, buffer);
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
		printf("Fib %02d (%s): %s\n", FibNum, BusAnalysers[i], buffer);

		// Free BUS
		resp = FeasaCom_Send(DEVPATH, "BUSFREE", buffer);
		if ( resp==-1 ) {
			printf("Unable to Free the BUS\n");
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		} else if ( resp==0 ) {
			printf(" Syntax error or Timeout detected!\n");
			FeasaCom_Close(DEVPATH);
			dlclose(hFeasaCom);
			return 1;
		}
	}

	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DEVPATH) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");

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
