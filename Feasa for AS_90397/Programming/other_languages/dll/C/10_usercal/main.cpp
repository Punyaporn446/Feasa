/**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: UserCal
*
*  DESCRIPTION: This example demonstrates how to use the
*  UserCal library embedded in the Feasa DLL in order to ease
*  the integration of the calibration process in any user
*  custom software.
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
#include <conio.h>
#include <string.h>
#include <time.h>
#include "feasacom.h"

#define TOFLASH 0 //Decides whether adjustments are stored in flash or RAM

// Declare pointers to call functions
tFeasaCom_IsPortAvailable FeasaCom_IsPortAvailable;
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;
tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;
tFeasaCom_GetBaudrate FeasaCom_GetBaudrate;
tFeasaCom_GetError_Description FeasaCom_GetError_Description;
tFeasaCom_Capture FeasaCom_Capture;
tFeasaCom_UserCal_ResetIntensity FeasaCom_UserCal_ResetIntensity;
tFeasaCom_UserCal_AdjustIntensity FeasaCom_UserCal_AdjustIntensity;
tFeasaCom_UserCal_ResetAbsInt FeasaCom_UserCal_ResetAbsInt;
tFeasaCom_UserCal_AdjustAbsInt FeasaCom_UserCal_AdjustAbsInt;
tFeasaCom_UserCal_ResetWavelengthOffset FeasaCom_UserCal_ResetWavelengthOffset;
tFeasaCom_UserCal_AdjustWavelengthOffset FeasaCom_UserCal_AdjustWavelengthOffset;
tFeasaCom_UserCal_ResetxyOffsets FeasaCom_UserCal_ResetxyOffsets;
tFeasaCom_UserCal_AdjustxyOffsets FeasaCom_UserCal_AdjustxyOffsets;
tFeasaCom_UserCal_GetIntensityGain FeasaCom_UserCal_GetIntensityGain;
tFeasaCom_UserCal_GetxyOffsets  FeasaCom_UserCal_GetxyOffsets;
tFeasaCom_UserCal_GetWavelengthOffset FeasaCom_UserCal_GetWavelengthOffset;
tFeasaCom_UserCal_GetAbsIntFactor FeasaCom_UserCal_GetAbsIntFactor;
tFeasaCom_UserCal_ResetRGBAdj  FeasaCom_UserCal_ResetRGBAdj;
tFeasaCom_UserCal_TakeRGBCurrentValues FeasaCom_UserCal_TakeRGBCurrentValues;
tFeasaCom_UserCal_AdjustRGB FeasaCom_UserCal_AdjustRGB;

void FormatDecimal(char * buffer);
int BalanceIntToAvg(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustAbsInt(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustWavelengthOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustxyOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int AdjustRGB(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes);
int ReadParams(int DevicePort, int nFibers);

int main(int argc, char* argv[])
{
	// Variables
	int resp; // Used to store the response from communication commands
	char command[100]; // Used to store the composed command
	int CommPort = 6;
	int NumFibers = 5;
	char Baudrate[20] = "AUTO";
	int i; // auxiliar
	double Hue; // to store Hue
	int Saturation; // to store Saturation
	long Intensity; // to store Intensity
	int capture_mode, isPWM, capture_range, capture_frames; //to store the options selected

	/* --- DLL ----------------------------------------------------------------*/
	// Load
	printf("Loading DLL...");
	HINSTANCE hFeasaComDLL=LoadLibrary("feasacom.dll");
	if(hFeasaComDLL==NULL) {
		printf(" Failed!\n");
		return 1;
	}
	printf(" OK!\n");
	// Get Addresses of DLL functions
	printf("Getting Function addresses...");
	FeasaCom_IsPortAvailable = (tFeasaCom_IsPortAvailable)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_IsPortAvailable");
	FeasaCom_Open = (tFeasaCom_Open)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Open");
	FeasaCom_Send = (tFeasaCom_Send)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send");
	FeasaCom_Close = (tFeasaCom_Close)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Close");
	FeasaCom_GetBaudrate = (tFeasaCom_GetBaudrate)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetBaudrate");
	FeasaCom_SetResponseTimeout = (tFeasaCom_SetResponseTimeout)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SetResponseTimeout");
	FeasaCom_GetError_Description = (tFeasaCom_GetError_Description)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetError_Description");
	FeasaCom_Capture = (tFeasaCom_Capture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Capture");
	FeasaCom_UserCal_ResetIntensity = (tFeasaCom_UserCal_ResetIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetIntensity");
	FeasaCom_UserCal_AdjustIntensity = (tFeasaCom_UserCal_AdjustIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustIntensity");
	FeasaCom_UserCal_ResetAbsInt = (tFeasaCom_UserCal_ResetAbsInt)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetAbsInt");
	FeasaCom_UserCal_AdjustAbsInt = (tFeasaCom_UserCal_AdjustAbsInt)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustAbsInt");
	FeasaCom_UserCal_ResetWavelengthOffset = (tFeasaCom_UserCal_ResetWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetWavelengthOffset");
	FeasaCom_UserCal_AdjustWavelengthOffset = (tFeasaCom_UserCal_AdjustWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustWavelengthOffset");
	FeasaCom_UserCal_ResetxyOffsets = (tFeasaCom_UserCal_ResetxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetxyOffsets");
	FeasaCom_UserCal_AdjustxyOffsets = (tFeasaCom_UserCal_AdjustxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustxyOffsets");
	FeasaCom_UserCal_GetIntensityGain = (tFeasaCom_UserCal_GetIntensityGain)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetIntensityGain");
	FeasaCom_UserCal_GetxyOffsets = (tFeasaCom_UserCal_GetxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetxyOffsets");
	FeasaCom_UserCal_GetWavelengthOffset = (tFeasaCom_UserCal_GetWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetWavelengthOffset");
	FeasaCom_UserCal_GetAbsIntFactor = (tFeasaCom_UserCal_GetAbsIntFactor)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetAbsIntFactor");
	FeasaCom_UserCal_ResetRGBAdj = (tFeasaCom_UserCal_ResetRGBAdj)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetRGBAdj");
	FeasaCom_UserCal_TakeRGBCurrentValues = (tFeasaCom_UserCal_TakeRGBCurrentValues)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_TakeRGBCurrentValues");
	FeasaCom_UserCal_AdjustRGB = (tFeasaCom_UserCal_AdjustRGB)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustRGB");
	if ((FeasaCom_IsPortAvailable == NULL) || (FeasaCom_Open == NULL) || (FeasaCom_Send == NULL) || (FeasaCom_Close == NULL) || (FeasaCom_GetBaudrate == NULL) || (FeasaCom_SetResponseTimeout == NULL) || (FeasaCom_GetError_Description == NULL) || (FeasaCom_Capture == NULL) || (FeasaCom_UserCal_ResetIntensity == NULL) || (FeasaCom_UserCal_AdjustIntensity == NULL) || (FeasaCom_UserCal_ResetAbsInt == NULL) || (FeasaCom_UserCal_AdjustAbsInt == NULL) || (FeasaCom_UserCal_ResetWavelengthOffset == NULL) || (FeasaCom_UserCal_AdjustWavelengthOffset == NULL) || (FeasaCom_UserCal_ResetxyOffsets == NULL) || (FeasaCom_UserCal_AdjustxyOffsets == NULL) || (FeasaCom_UserCal_GetIntensityGain == NULL) || (FeasaCom_UserCal_GetxyOffsets == NULL) || (FeasaCom_UserCal_GetWavelengthOffset == NULL) || (FeasaCom_UserCal_GetAbsIntFactor == NULL) || (FeasaCom_UserCal_ResetRGBAdj == NULL) || (FeasaCom_UserCal_TakeRGBCurrentValues == NULL) || (FeasaCom_UserCal_AdjustRGB == NULL)) {
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

	//Increase the maximum timeout to avoid errors caused by long captures (FeasaCom_SetResponseTimeout)
	FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

	//Allow the user to choose the test settings
	printf("\nPlease, enter COM Port:\n");
	for (i = 1; i<100; i++)
		if (FeasaCom_IsPortAvailable(i) == 1)
			printf("  %d: COM%d\n", i, i);
	printf("  Your selection:");
	scanf("%d", &CommPort);
	if ((CommPort<1) || (CommPort>255)) CommPort = 1;

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
		case 1: strcpy(Baudrate, "9600"); break;
		case 2: strcpy(Baudrate, "19200"); break;
		case 3: strcpy(Baudrate, "38400"); break;
		case 4: strcpy(Baudrate, "57600"); break;
		case 5: strcpy(Baudrate, "115200"); break;
		case 6: strcpy(Baudrate, "230400"); break;
		case 7: strcpy(Baudrate, "460800"); break;
		case 8: strcpy(Baudrate, "921600"); break;
		default: strcpy(Baudrate, "AUTO"); break;
	}

	//Open port where the LED Analyser is connected
	printf("Opening port...");
	if (!FeasaCom_Open(CommPort, Baudrate)) {
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
		printf("Baudrate: %ld\n", FeasaCom_GetBaudrate(CommPort));

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
			BalanceIntToAvg(CommPort, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 2) {
			AdjustAbsInt(CommPort, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 3) {
			AdjustWavelengthOffsets(CommPort, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 4) {
			AdjustxyOffsets(CommPort, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 5) {
			AdjustRGB(CommPort, NumFibers, isPWM, capture_range, capture_frames);
		}
		else if (i == 6) {
			ReadParams(CommPort, NumFibers);
		}
		else {
			break;
		}

	}

	// Close port
	printf("Closing port...");
	if ( !FeasaCom_Close(CommPort) ) {
		printf(" Failed!\n");
		FreeLibrary((HMODULE)hFeasaComDLL);
		return 1;
	}
	printf(" OK!\n");

	FreeLibrary((HMODULE)hFeasaComDLL);
	return 0;
}

int BalanceIntToAvg(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes)
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
		FeasaCom_UserCal_ResetIntensity(DevicePort, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Read back intensities / Calculates average intensity
	int AvgInt = 0;
	for (i = 1; i <= nFibers; i++)
	{
		sprintf(command, "GETINTENSITY%02d", i);
		resp = FeasaCom_Send(DevicePort, command, buffer);
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
		resp = FeasaCom_UserCal_AdjustIntensity(DevicePort, i, AvgInt, isPWM, CaptureRange, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error: %s\n", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETINTENSITYALL", buffer);
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

int AdjustAbsInt(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes)
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
		FeasaCom_UserCal_ResetAbsInt(DevicePort, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustAbsInt(DevicePort, i, AbsIntRef, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETABSINTALL", buffer);
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

int AdjustWavelengthOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes)
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
		FeasaCom_UserCal_ResetWavelengthOffset(DevicePort, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustWavelengthOffset(DevicePort, i, Wref, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETwavelengthALL", buffer);
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

int AdjustxyOffsets(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes)
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
		FeasaCom_UserCal_ResetxyOffsets(DevicePort, i, TOFLASH);

	//Capture
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

	//Calibration
	for (i = 1; i <= nFibers; i++)
	{
		resp = FeasaCom_UserCal_AdjustxyOffsets(DevicePort, i, xRef, yRef, TOFLASH);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETxyALL", buffer);
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

int AdjustRGB(int DevicePort, int nFibers, int isPWM, int CaptureRange, int PWMframes)
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
		FeasaCom_UserCal_ResetRGBAdj(DevicePort, i);

	printf("Measuring reference values...\n");
	for (c = 0; c < 3; c++) {
		printf("Please, switch on %s LED and press a key to continue...\n", COLORS[c]);
		resp = getch();

		//Capture
		FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes);

		//Store current measurements
		for (i = 1; i <= nFibers; i++)
		{
			resp = FeasaCom_UserCal_TakeRGBCurrentValues(DevicePort, i, (*COLORS[c]));
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
		resp = FeasaCom_UserCal_AdjustRGB(DevicePort, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB);
		if (resp != 1)
		{
			FeasaCom_GetError_Description(buffer);
			printf("Error:%s", buffer);
			return 0;
		}
	}

	//Check results
	FeasaCom_Capture(DevicePort, isPWM, CaptureRange, PWMframes); //Capture
	resp = FeasaCom_Send(DevicePort, "GETxyALL", buffer);
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
	resp = FeasaCom_Send(DevicePort, "GETABSINTALL", buffer);
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

int ReadParams(int DevicePort, int nFibers)
{
	char buffer[1024];
	int i;
	int Gain = 0;
	float xOffset = 0, yOffset = 0;
	int WavelengthOffset = 0;
	double AbsIntFactor = 0;

	//Retrieve Intensity gains
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetIntensityGain(DevicePort, i, &Gain);
		printf("Int Gain %d: %d\n", i, Gain);
	}

	//Retrieve xy Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetxyOffsets(DevicePort, i, &xOffset, &yOffset);
		printf("xy Offsets %d: %0.4f; %0.4f\n", i, xOffset, yOffset);
	}

	//Retrieve Wavelength Offsets
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetWavelengthOffset(DevicePort, i, &WavelengthOffset);
		printf("Wl Offsets %d: %d\n", i, WavelengthOffset);
	}

	//Retrieve Abs Int Factor
	for (i = 1; i <= nFibers; i++)
	{
		FeasaCom_UserCal_GetAbsIntFactor(DevicePort, i, &AbsIntFactor);
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
