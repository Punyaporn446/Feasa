/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: UserCal
*
*  DESCRIPTION: This example demonstrates how to use the
*  UserCal library embedded in the Feasa SO Library in order
*  to ease the integration of the calibration process in any
*  user custom software.
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

#define TOFLASH 0 //Decides whether adjustments are stored in flash or RAM

// Declare pointers to call functions
tFeasaComIsPortAvailable FeasaCom_IsPortAvailable;
tFeasaComOpen FeasaCom_Open;
tFeasaComSend FeasaCom_Send;
tFeasaComClose FeasaCom_Close;
tFeasaComSetResponseTimeout FeasaCom_SetResponseTimeout;
tFeasaComGetBaudrate FeasaCom_GetBaudrate;
tFeasaComGetErrorDescription FeasaCom_GetError_Description;
tFeasaComCapture FeasaCom_Capture;
tFeasaComUserCal_ResetIntensity FeasaCom_UserCal_ResetIntensity;
tFeasaComUserCal_AdjustIntensity FeasaCom_UserCal_AdjustIntensity;
tFeasaComUserCal_ResetAbsInt FeasaCom_UserCal_ResetAbsInt;
tFeasaComUserCal_AdjustAbsInt FeasaCom_UserCal_AdjustAbsInt;
tFeasaComUserCal_ResetWavelengthOffset FeasaCom_UserCal_ResetWavelengthOffset;
tFeasaComUserCal_AdjustWavelengthOffset FeasaCom_UserCal_AdjustWavelengthOffset;
tFeasaComUserCal_ResetxyOffsets FeasaCom_UserCal_ResetxyOffsets;
tFeasaComUserCal_AdjustxyOffsets FeasaCom_UserCal_AdjustxyOffsets;
tFeasaComUserCal_GetIntensityGain FeasaCom_UserCal_GetIntensityGain;
tFeasaComUserCal_GetxyOffsets  FeasaCom_UserCal_GetxyOffsets;
tFeasaComUserCal_GetWavelengthOffset FeasaCom_UserCal_GetWavelengthOffset;
tFeasaComUserCal_GetAbsIntFactor FeasaCom_UserCal_GetAbsIntFactor;
tFeasaComUserCal_ResetRGBAdj  FeasaCom_UserCal_ResetRGBAdj;
tFeasaComUserCal_TakeRGBCurrentValues FeasaCom_UserCal_TakeRGBCurrentValues;
tFeasaComUserCal_AdjustRGB FeasaCom_UserCal_AdjustRGB;

void FormatDecimal(char * buffer);
int BalanceIntToAvg(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustAbsInt(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustWavelengthOffsets(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustxyOffsets(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustRGB(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int ReadParams(char * DevPath, int nFibers);

int main(int argc, char* argv[])
{
	// Variables
	char buffer[500];
	char DevPath[255]; //To store device path
	int NumFibers = 5;
	int Baudrate = 0;
	int i; // auxiliar
	int n;
	int capture_mode, isPWM, capture_range, capture_frames; //to store the options selected
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
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of SO Library functions
	printf("Getting Function addresses...");
	FeasaCom_IsPortAvailable = (tFeasaComIsPortAvailable)dlsym(hFeasaCom, "FeasaCom_IsPortAvailable");
	FeasaCom_Open = (tFeasaComOpen)dlsym(hFeasaCom, "FeasaCom_Open");
	FeasaCom_Send = (tFeasaComSend)dlsym(hFeasaCom, "FeasaCom_Send");
	FeasaCom_Close = (tFeasaComClose)dlsym(hFeasaCom, "FeasaCom_Close");
	FeasaCom_GetBaudrate = (tFeasaComGetBaudrate)dlsym(hFeasaCom, "FeasaCom_GetBaudrate");
	FeasaCom_SetResponseTimeout = (tFeasaComSetResponseTimeout)dlsym(hFeasaCom, "FeasaCom_SetResponseTimeout");
	FeasaCom_GetError_Description = (tFeasaComGetErrorDescription)dlsym(hFeasaCom, "FeasaCom_GetError_Description");
	FeasaCom_Capture = (tFeasaComCapture)dlsym(hFeasaCom, "FeasaCom_Capture");
	FeasaCom_UserCal_ResetIntensity = (tFeasaComUserCal_ResetIntensity)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetIntensity");
	FeasaCom_UserCal_AdjustIntensity = (tFeasaComUserCal_AdjustIntensity)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustIntensity");
	FeasaCom_UserCal_ResetAbsInt = (tFeasaComUserCal_ResetAbsInt)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetAbsInt");
	FeasaCom_UserCal_AdjustAbsInt = (tFeasaComUserCal_AdjustAbsInt)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustAbsInt");
	FeasaCom_UserCal_ResetWavelengthOffset = (tFeasaComUserCal_ResetWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetWavelengthOffset");
	FeasaCom_UserCal_AdjustWavelengthOffset = (tFeasaComUserCal_AdjustWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustWavelengthOffset");
	FeasaCom_UserCal_ResetxyOffsets = (tFeasaComUserCal_ResetxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetxyOffsets");
	FeasaCom_UserCal_AdjustxyOffsets = (tFeasaComUserCal_AdjustxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustxyOffsets");
	FeasaCom_UserCal_GetIntensityGain = (tFeasaComUserCal_GetIntensityGain)dlsym(hFeasaCom, "FeasaCom_UserCal_GetIntensityGain");
	FeasaCom_UserCal_GetxyOffsets = (tFeasaComUserCal_GetxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_GetxyOffsets");
	FeasaCom_UserCal_GetWavelengthOffset = (tFeasaComUserCal_GetWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_GetWavelengthOffset");
	FeasaCom_UserCal_GetAbsIntFactor = (tFeasaComUserCal_GetAbsIntFactor)dlsym(hFeasaCom, "FeasaCom_UserCal_GetAbsIntFactor");
	FeasaCom_UserCal_ResetRGBAdj = (tFeasaComUserCal_ResetRGBAdj)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetRGBAdj");
	FeasaCom_UserCal_TakeRGBCurrentValues = (tFeasaComUserCal_TakeRGBCurrentValues)dlsym(hFeasaCom, "FeasaCom_UserCal_TakeRGBCurrentValues");
	FeasaCom_UserCal_AdjustRGB = (tFeasaComUserCal_AdjustRGB)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustRGB");
	if ((FeasaCom_IsPortAvailable == NULL) || (FeasaCom_Open == NULL) || (FeasaCom_Send == NULL) || (FeasaCom_Close == NULL) || (FeasaCom_GetBaudrate == NULL) || (FeasaCom_SetResponseTimeout == NULL) || (FeasaCom_GetError_Description == NULL) || (FeasaCom_Capture == NULL) || (FeasaCom_UserCal_ResetIntensity == NULL) || (FeasaCom_UserCal_AdjustIntensity == NULL) || (FeasaCom_UserCal_ResetAbsInt == NULL) || (FeasaCom_UserCal_AdjustAbsInt == NULL) || (FeasaCom_UserCal_ResetWavelengthOffset == NULL) || (FeasaCom_UserCal_AdjustWavelengthOffset == NULL) || (FeasaCom_UserCal_ResetxyOffsets == NULL) || (FeasaCom_UserCal_AdjustxyOffsets == NULL) || (FeasaCom_UserCal_GetIntensityGain == NULL) || (FeasaCom_UserCal_GetxyOffsets == NULL) || (FeasaCom_UserCal_GetWavelengthOffset == NULL) || (FeasaCom_UserCal_GetAbsIntFactor == NULL) || (FeasaCom_UserCal_ResetRGBAdj == NULL) || (FeasaCom_UserCal_TakeRGBCurrentValues == NULL) || (FeasaCom_UserCal_AdjustRGB == NULL)) {
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
	n = 0;
	printf("\nPlease, choose Port:\n");
	for (i = 0; i<20; i++) {
        sprintf(buffer, "/dev/ttyUSB%d", i);
		if (FeasaCom_IsPortAvailable(buffer) == 1) {
			printf("  %d: %s\n", i, buffer);
			n++;
        }
    }
    if (n==0) {
		printf(" No ports have been detected!\n");
		return 1;
    }
	printf("  Your selection:");
	scanf("%d", &i);
	if ((i<0) || (i>255)) i = 0;
	sprintf(DevPath, "/dev/ttyUSB%d", i);

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
		case 1: Baudrate=9600; break;
		case 2: Baudrate=19200; break;
		case 3: Baudrate=38400; break;
		case 4: Baudrate=57600; break;
		case 5: Baudrate=115200; break;
		case 6: Baudrate=230400; break;
		case 7: Baudrate=460800; break;
		case 8: Baudrate=921600; break;
		default: Baudrate=0; break;
	}

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!FeasaCom_Open(DevPath, Baudrate)) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");

	//Allow the user to choose the test settings
	printf("\nPlease, enter number of fibers:");
	scanf("%d", &NumFibers);
	if ((NumFibers<1) || (NumFibers>20)) NumFibers = 20;

	//Allow the user to choose the test settings
	printf("\nPlease, select the Capture Mode:\n");
	printf("  0: AUTO\n");
	printf("  1: MANUAL\n");
	printf("  2: PWM: AUTO-RANGE & AUTO-FRAMING\n");
	printf("  3: PWM: MANUAL-RANGE & AUTO-FRAMING\n");
	printf("  4: PWM: AUTO-RANGE & MANUAL-FRAMING\n");
	printf("  5: PWM: MANUAL-RANGE & MANUAL-FRAMING\n");
	printf("  Your selection:");
	scanf("%d", &capture_mode);
	if ((capture_mode<0) || (capture_mode>5)) capture_mode = 0;

	if ((capture_mode == 1) || (capture_mode == 3) || (capture_mode == 5)) {
		printf("\nPlease, select the Capture Range:\n");
		printf("  1: LOW\n");
		printf("  2: MEDIUM\n");
		printf("  3: HIGH\n");
		printf("  4: SUPER\n");
		printf("  5: ULTRA\n");
		printf("  Your selection:");
		scanf("%d", &capture_range);
		if ((capture_range<1) || (capture_range>5)) capture_range = 3;
	}

	if ((capture_mode == 4) || (capture_mode == 5)) {
		printf("\nPlease, select the number of Frames for the averaging (from 1 to 15): ");
		scanf("%d", &capture_frames);
		if ((capture_frames<1) || (capture_frames>15)) capture_frames = 5;
	}
	printf("\n\n");

	if (capture_mode == 1) {
		isPWM = 0;
	}
	else if (capture_mode == 2) {
		isPWM = 1;
		capture_range = 0;
		capture_frames = 0;
	}
	else if (capture_mode == 3) {
		isPWM = 1;
		capture_frames = 0;
	}
	else if (capture_mode == 4) {
		isPWM = 1;
		capture_range = 0;
	}
	else if (capture_mode == 5) {
		isPWM = 1;
	}
	else {
		isPWM = 0;
		capture_range = 0;
	}

	if (i == 0)
		printf("Baudrate: %ld\n", FeasaCom_GetBaudrate(DevPath));

	while (1) {
		//Allow the user to choose the test settings
		printf("\nPlease, choose an action:\n");
		printf("  1: Balance Int to Avg\n");
		printf("  2: Adjust Abs Int\n");
		printf("  3: Adjust Wavelength\n");
		printf("  4: Adjust xy\n");
		printf("  5: Adjust RGB\n");
		printf("  6: Read back params\n");
		printf("  0: EXIT\n");
		printf("  Your selection:");
		scanf("%d", &i);

		if (i == 1) {
			BalanceIntToAvg(DevPath, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 2) {
			AdjustAbsInt(DevPath, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 3) {
			AdjustWavelengthOffsets(DevPath, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 4) {
			AdjustxyOffsets(DevPath, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 5) {
			AdjustRGB(DevPath, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 6) {
			ReadParams(DevPath, NumFibers);
		}
		else {
			break;
		}

	}

	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(DevPath) ) {
		printf(" Failed!\n");
		dlclose(hFeasaCom);
		return 1;
	}
	printf(" OK!\n");

	dlclose(hFeasaCom);
	return 0;
}

int BalanceIntToAvg(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes)
{
	int resp;
	char buffer[1024]; // Used to store the response from the LED Analyser
	int i;
	int auxint = 0;
	char command[100];

	//-------------------------------------------------
	// RELATIVE INTENSITY ADJUSTMENT
	//-------------------------------------------------

	//Reset intensities
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetIntensity(DevPath, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

	//Read back intensities / Calculates average intensity
	int AvgInt = 0;
	for (i = 1; i <= nFibers; i++)
	{
		sprintf(command, "GETINTENSITY%02d", i);
		resp = FeasaCom_Send(DevPath, command, buffer);
		if (resp == -1)
		{
			printf("Error: unable to send the command!");
			return 0;
		}
		else if (resp == 0)
		{
			printf("Error: Timeout detected!");
			return 0;
		}
		sscanf(buffer, "%d", &auxint);
		AvgInt += auxint;
	}
	AvgInt = AvgInt / nFibers;
	printf("AvgInt=%d\n", AvgInt);

	//Adjustment
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustIntensity(DevPath, i, AvgInt, isPWM, CaptureRange, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error: %s\n", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevPath, "GETINTENSITYALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Error: Timeout detected!\n");
		return 0;
	}
	printf("Results: %s\n", buffer);

	return 1;
}

int AdjustAbsInt(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes)
{
	int resp;
	char buffer[1024];
	int i;

	//Read reference
	float AbsIntRef = 2.355E-02;
	printf("\n  Enter Absolute Intensity Reference value (ex: 2.355E-02):");
	try {
		scanf("%f", &AbsIntRef);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("AbsIntRef=%0.5E\n", AbsIntRef);

	//-------------------------------------------------
	// ABSOLUTE INTENSITY ADJUSTMENT
	//-------------------------------------------------

	//Reset factors
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetAbsInt(DevPath, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustAbsInt(DevPath, i, AbsIntRef, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevPath, "GETABSINTALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Timeout detected!\n");
		return 0;
	}
	printf("%s\n", buffer);

	return 1;
}

int AdjustWavelengthOffsets(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes)
{
	int resp;
	char buffer[1024];
	int i;

	//Read reference
	int Wref = 0;
	printf("\n  Enter Wavelength Reference value (ex: 633):");
	try {
		scanf("%d", &Wref);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("Wref=%dnm\n", Wref);

	//-------------------------------------------------
	// WAVELENGTH OFFSETS ADJUSTMENT
	//-------------------------------------------------

	//Reset offsets
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetWavelengthOffset(DevPath, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustWavelengthOffset(DevPath, i, Wref, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevPath, "GETwavelengthALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Timeout detected!\n");
		return 0;
	}
	printf("%s\n", buffer);

	return 1;
}

int AdjustxyOffsets(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes)
{
	int resp;
	char buffer[1024];
	int i;

	float xRef = 0, yRef = 0;
	printf("\n  Enter Reference value for 'x' (ex: 0.3482):");
	try {
		scanf("%f", &xRef);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'y' (ex: 0.3482):");
	try {
		scanf("%f", &yRef);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("xRef=%0.4f\n", xRef);
	printf("yRef=%0.4f\n", yRef);

	//-------------------------------------------------
	// xy OFFSETS ADJUSTMENT
	//-------------------------------------------------

	//Reset offsets
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetxyOffsets(DevPath, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustxyOffsets(DevPath, i, xRef, yRef, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevPath, "GETxyALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Timeout detected!\n");
		return 0;
	}
	printf("%s\n", buffer);

	return 1;
}

int AdjustRGB(char * DevPath, int nFibers, int isPWM, int CaptureRange, int PWMframes)
{
	int resp;
	char buffer[1024];
	int i, c;
	const char * COLORS[] = { "RED", "GREEN", "BLUE" };

	float xRefR = 0, yRefR = 0, xRefG = 0, yRefG = 0, xRefB = 0, yRefB = 0;
	double AbsIntRefR = 0, AbsIntRefG = 0, AbsIntRefB = 0;
	printf("\n  Enter Reference value for 'x (RED)' (ex: 0.3482):");
	try {
		scanf("%f", &xRefR);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'y (RED)' (ex: 0.3482):");
	try {
		scanf("%f", &yRefR);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'Abs Int (RED)' (ex: 0.3482):");
	try {
		scanf("%lf", &AbsIntRefR);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'x (GREEN)' (ex: 0.3482):");
	try {
		scanf("%f", &xRefG);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'y (GREEN)' (ex: 0.3482):");
	try {
		scanf("%f", &yRefG);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'Abs Int (GREEN)' (ex: 0.3482):");
	try {
		scanf("%lf", &AbsIntRefG);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'x (BLUE)' (ex: 0.3482):");
	try {
		scanf("%f", &xRefB);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'y (BLUE)' (ex: 0.3482):");
	try {
		scanf("%f", &yRefB);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("\n  Enter Reference value for 'Abs Int (BLUE)' (ex: 0.3482):");
	try {
		scanf("%lf", &AbsIntRefB);
	}
	catch (int ex) { printf("Wrong value!\n"); }
	printf("xRef (RED)=%0.4f\n", xRefR);
	printf("yRef (RED)=%0.4f\n", yRefR);
	printf("AbsIntRef (RED)=%lf\n", AbsIntRefR);
	printf("xRef (GREEN)=%0.4f\n", xRefG);
	printf("yRef (GREEN)=%0.4f\n", yRefG);
	printf("AbsIntRef (GREEN)=%lf\n", AbsIntRefG);
	printf("xRef (BLUE)=%0.4f\n", xRefB);
	printf("yRef (BLUE)=%0.4f\n", yRefB);
	printf("AbsIntRef (BLUE)=%lf\n", AbsIntRefB);

	//-------------------------------------------------
	// RGB ADJUSTMENT
	//-------------------------------------------------

	//Reset
	printf("Resetting...\n");
	for (i = 1; i <= nFibers; i++)
		FeasaCom_UserCal_ResetRGBAdj(DevPath, i);

    resp = getchar(); //Clear stdin from latest scanf

	printf("Measuring reference values...\n");
	for (c = 0; c < 3; c++) {
		printf("Please, switch on %s LED and press Enter to continue...\n", COLORS[c]);
		resp = 0;
		while ( (resp = getchar()) != '\n' && resp != EOF ) {  }

		//Capture
		FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes);

		//Store current measurements
		for (i = 1; i <= nFibers; i++)
		{
			resp = FeasaCom_UserCal_TakeRGBCurrentValues(DevPath, i, (*COLORS[c]));
			if (resp != 1)
			{
				FeasaCom_GetError_Description(buffer);
				printf("Error:%s", buffer);
				return 0;
			}
		}
	}

	//Adjustment
	printf("Adjusting...\n");
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustRGB(DevPath, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevPath, "GETxyALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Timeout detected!\n");
		return 0;
	}
	printf("%s\n", buffer);
	resp = FeasaCom_Send(DevPath, "GETABSINTALL", buffer);
	if (resp == -1)
	{
		printf("Error! unable to send the command!\n");
		return 0;
	}
	else if (resp == 0)
	{
		printf("Timeout detected!\n");
		return 0;
	}
	printf("%s\n", buffer);

	return 1;
}

int ReadParams(char * DevPath, int nFibers)
{
	int i;
	int Gain = 0;
	float xOffset = 0, yOffset = 0;
	int WavelengthOffset = 0;
	double AbsIntFactor = 0;

	//Retrieve Intensity gains
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetIntensityGain(DevPath, i, &Gain);
		printf("Int Gain %d: %d\n", i, Gain);
	}

	//Retrieve xy Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetxyOffsets(DevPath, i, &xOffset, &yOffset);
		printf("xy Offsets %d: %0.4f; %0.4f\n", i, xOffset, yOffset);
	}

	//Retrieve Wavelength Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetWavelengthOffset(DevPath, i, &WavelengthOffset);
		printf("Wl Offsets %d: %d\n", i, WavelengthOffset);
	}

	//Retrieve Abs Int Factor
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetAbsIntFactor(DevPath, i, &AbsIntFactor);
		printf("Abs Int Factor %d: %E\n", i, AbsIntFactor);
	}

	return 1;
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
