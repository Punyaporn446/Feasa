#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

	// Basic Comm functions
	extern int __stdcall FeasaCom_Open(const int CommPort, const char * Baudrate);
	extern int __stdcall FeasaCom_Send(const int CommPort, const char * Command, char * ResponseText);
	extern int __stdcall FeasaCom_Close(const int CommPort);
	extern int __stdcall FeasaCom_OpenSN(const char * SerialNumber, const char * Baudrate);
	extern int __stdcall FeasaCom_SendSN(const char * SerialNumber, const char * Command, char * ResponseText);
	extern int __stdcall FeasaCom_CloseSN(const char * SerialNumber);

	// Comm helper functions
	extern int __stdcall FeasaCom_Capture(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
	extern int __stdcall FeasaCom_CaptureFromAll(int * ReturnValues, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
	extern int __stdcall FeasaCom_SendToAll(int * ReturnValues, const const char * Command, char ** Responses);
	extern int __stdcall FeasaCom_SendToAll_NR(int * ReturnValues, const const char * Command);
	extern int __stdcall FeasaCom_Open_Multi(int * ReturnValues, int * CommPorts, const int nPorts, const char * Baudrate);
	extern int __stdcall FeasaCom_Close_Multi(int * ReturnValues, int * CommPorts, const int nPorts);
	extern int __stdcall FeasaCom_Send_Multi(int * ReturnValues, int * CommPorts, const int nPorts, const char ** Commands, char ** Responses);
	extern int __stdcall FeasaCom_Send_Multi_NR(int * ReturnValues, int * CommPorts, const int nPorts, const char * Commands, const char CommandSeparator);
	extern int __stdcall FeasaCom_OpenSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char * Baudrate);
	extern int __stdcall FeasaCom_CloseSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials);
	extern int __stdcall FeasaCom_SendSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char ** Commands, char ** Responses);
	extern int __stdcall FeasaCom_CloseAll(void);
	extern int __stdcall GetResponseByPort(const int CommPort, char * ResponseText);

	// Daisy-chain functions
	extern int __stdcall FeasaCom_DaisyChain_Add(const int CommPort, const char * SerialNumber);
	extern int __stdcall FeasaCom_DaisyChain_Del(const int CommPort, const char * SerialNumber);
	extern int __stdcall FeasaCom_DaisyChain_Clear(const int CommPort);
	extern int __stdcall FeasaCom_DaisyChain_Capture(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);

	// Comm handling functions
	extern int __stdcall FeasaCom_EnumPorts(void);
	extern int __stdcall FeasaCom_IsConnected(const char * SerialNumber, const char * Baudrate);
	extern int __stdcall FeasaCom_AreConnected(const char ** SerialNumbers, const int nSerials, const char * Baudrate, int * PortNumbers);
	extern int __stdcall FeasaCom_AreConnectedS(const char * SerialNumbers, const char * Baudrate, int * PortNumbers);
	extern int __stdcall FeasaCom_Detect(int * CommPorts, const char * Baudrate);
	extern int __stdcall FeasaCom_DetectSN(char ** SerialNumbers, const char * Baudrate);
	extern int __stdcall FeasaCom_IsPortAvailable(const int CommPort);
	extern int __stdcall FeasaCom_ListPortsDetected(int * ListOfPortsDetected);
	extern int __stdcall FeasaCom_ListPortsDetectedTxt(char * ListOfPortsDetected, const char * Delimiter);
	extern int __stdcall FeasaCom_SetResponseTimeout(const unsigned int Timeout);
	extern long __stdcall FeasaCom_GetBaudrate(const int CommPort);
	extern void __stdcall FeasaCom_GetError_Description(char * ErrorDescription);
	extern int __stdcall FeasaCom_GetPortBySN(const char * SerialNumber);
	extern int __stdcall FeasaCom_GetSNByPort(char * SerialNumber, const int CommPort);

	// Binning
	extern int __stdcall FeasaCom_Binning_GetBinFromVECFile(const char * Path, const float x, const float y, char * ResultBinName);

	// UserCal functions
	extern int __stdcall FeasaCom_UserCal_ResetxyOffsets(const int CommPort, const int Fiber, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_SetxyOffsets(const int CommPort, const int Fiber, const float xOffset, const float yOffset, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetxyOffsets(const int CommPort, const int Fiber, float * xOffset, float * yOffset);
	extern int __stdcall FeasaCom_UserCal_AdjustxyOffsets(const int CommPort, const int Fiber, const float xRef, const float yRef, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetWavelengthOffset(const int CommPort, const int Fiber, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetWavelengthOffset(const int CommPort, const int Fiber, int * WavelengthOffset);
	extern int __stdcall FeasaCom_UserCal_SetWavelengthOffset(const int CommPort, const int Fiber, const int WavelengthOffset, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustWavelengthOffset(const int CommPort, const int Fiber, const int WavelengthRef, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetIntensity(const int CommPort, const int Fiber, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetIntensityGain(const int CommPort, const int Fiber, int * Gain);
	extern int __stdcall FeasaCom_UserCal_SetIntensityGain(const int CommPort, const int Fiber, const int Gain, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustIntensity(const int CommPort, const int Fiber, const int IntensityRef, const int isPWM, const int CaptureRange, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetAbsInt(const int CommPort, const int Fiber, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_GetAbsIntFactor(const int CommPort, const int Fiber, double * Factor);
	extern int __stdcall FeasaCom_UserCal_SetAbsIntFactor(const int CommPort, const int Fiber, const double Factor, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_AdjustAbsInt(const int CommPort, const int Fiber, const double AbsIntRef, const int toFlash);
	extern int __stdcall FeasaCom_UserCal_ResetRGBAdj(const int CommPort, const int Fiber);
	extern int __stdcall FeasaCom_UserCal_TakeRGBCurrentValues(const int CommPort, const int Fiber, const char Color);
	extern int __stdcall FeasaCom_UserCal_AdjustRGB(const int CommPort, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

#endif //FEASACOM_H
