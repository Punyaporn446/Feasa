#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

	// Basic Comm functions
	extern int __stdcall FeasaCom_Open(int CommPort, char * Baudrate);
	extern int __stdcall FeasaCom_Close(int CommPort);
	extern int __stdcall FeasaCom_Send(int CommPort, char * Command, char * ResponseText);
	extern int __stdcall FeasaCom_OpenSN(char * SerialNumber, char * Baudrate);
	extern int __stdcall FeasaCom_CloseSN(char * SerialNumber);
	extern int __stdcall FeasaCom_SendSN(char * SerialNumber, char * Command, char * ResponseText);

	// Comm helper functions
	extern int __stdcall FeasaCom_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);
	extern int __stdcall FeasaCom_CaptureFromAll(int * ReturnValues, int isPWM, int CaptureRange, int CapturePWMFrames);
	extern int __stdcall FeasaCom_SendToAll(int * ReturnValues, const char * Command, char ** Responses);
	extern int __stdcall FeasaCom_CloseAll(void);
	extern int __stdcall FeasaCom_Open_Multi(int * ReturnValues, int * CommPorts, int nPorts, char * Baudrate);
	extern int __stdcall FeasaCom_Close_Multi(int * ReturnValues, int * CommPorts, int nPorts);
	extern int __stdcall FeasaCom_OpenSN_Multi(int * ReturnValues, char ** SerialNumbers, int nPorts, char * Baudrate);
	extern int __stdcall FeasaCom_CloseSN_Multi(int * ReturnValues, char ** SerialNumbers, int nPorts);

	// Daisy-chain functions
	extern int __stdcall FeasaCom_DaisyChain_Add(int CommPort, char * SerialNumber);
	extern int __stdcall FeasaCom_DaisyChain_Del(int CommPort, char * SerialNumber);
	extern int __stdcall FeasaCom_DaisyChain_Clear(int CommPort);
	extern int __stdcall FeasaCom_DaisyChain_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

	// Comm handling functions
	extern int __stdcall FeasaCom_EnumPorts(void);
	extern int __stdcall FeasaCom_IsConnected(char * SerialNumber, char * Baudrate);
	extern int __stdcall FeasaCom_AreConnected(char ** SerialNumbers, int nSerials, char * Baudrate, int * PortNumbers);
	extern int __stdcall FeasaCom_AreConnectedS(char * SerialNumbers, char * Baudrate, int * PortNumbers);
	extern int __stdcall FeasaCom_Detect(int * CommPorts, char * Baudrate);
	extern int __stdcall FeasaCom_DetectSN(char ** SerialNumbers, char * Baudrate);
	extern int __stdcall FeasaCom_IsPortAvailable(int CommPort);
	extern int __stdcall FeasaCom_ListPortsDetected(int * ListOfPortsDetected);
	extern int __stdcall FeasaCom_ListPortsDetectedTxt(char * ListOfPortsDetected, const char * Delimiter);
	extern int __stdcall FeasaCom_SetResponseTimeout(unsigned int Timeout);
	extern long __stdcall FeasaCom_GetBaudrate(int CommPort);
	extern void __stdcall FeasaCom_GetError_Description(char * ErrorDescription);
	extern int __stdcall FeasaCom_GetPortBySN(char * SerialNumber);
	extern int __stdcall FeasaCom_GetSNByPort(char * SerialNumber, int CommPort);

	// Binning
	extern int __stdcall FeasaCom_Binning_GetBinFromVECFile(char * Path, float x, float y, char * ResultBinName);

	// UserCal functions
	extern int __stdcall FeasaCom_UserCal_ResetxyOffsets(int CommPort, int Fiber, int toFlash);
	extern int __stdcall FeasaCom_UserCal_SetxyOffsets(int CommPort, int Fiber, float xOffset, float yOffset, int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetxyOffsets(int CommPort, int Fiber, float * xOffset, float * yOffset);
	extern int __stdcall FeasaCom_UserCal_AdjustxyOffsets(int CommPort, int Fiber, float xRef, float yRef, int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetWavelengthOffset(int CommPort, int Fiber, int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetWavelengthOffset(int CommPort, int Fiber, int * WavelengthOffset);
	extern int __stdcall FeasaCom_UserCal_SetWavelengthOffset(int CommPort, int Fiber, int WavelengthOffset, int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustWavelengthOffset(int CommPort, int Fiber, int WavelengthRef, int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetIntensity(int CommPort, int Fiber, int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetIntensityGain(int CommPort, int Fiber, int * Gain);
	extern int __stdcall FeasaCom_UserCal_SetIntensityGain(int CommPort, int Fiber, int Gain, int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustIntensity(int CommPort, int Fiber, int IntensityRef, int isPWM, int CaptureRange, int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetAbsInt(int CommPort, int Fiber, int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetAbsIntFactor(int CommPort, int Fiber, double * Factor);
	extern int __stdcall FeasaCom_UserCal_SetAbsIntFactor(int CommPort, int Fiber, double Factor, int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustAbsInt(int CommPort, int Fiber, double AbsIntRef, int toFlash);

#endif //FEASACOM_H
