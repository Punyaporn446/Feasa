#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

// ------ Definitions to use with LoadLibrary() ------

// Basic Comm functions
typedef int(__stdcall *tFeasaCom_OpenSN)(char * SerialNumber, char * Baudrate);
typedef int(__stdcall *tFeasaCom_CloseSN)(char * SerialNumber);
typedef int(__stdcall *tFeasaCom_SendSN)(char * SerialNumber, char * Command, char * ResponseText);
typedef int(__stdcall *tFeasaCom_Open)(int CommPort, char * Baudrate);
typedef int(__stdcall *tFeasaCom_Close)(int CommPort);
typedef int(__stdcall *tFeasaCom_Send)(int CommPort, char * Command, char * ResponseText);

// Comm helper functions
typedef int(__stdcall *tFeasaCom_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);
typedef int(__stdcall *tFeasaCom_CaptureFromAll)(int * ReturnValues, int isPWM, int CaptureRange, int CapturePWMFrames);
typedef int(__stdcall *tFeasaCom_SendToAll)(int * ReturnValues, const char * Command, char ** Responses);
typedef int(__stdcall *tFeasaCom_CloseAll)(void);

// Daisy-chain functions
typedef int(__stdcall *tFeasaCom_DaisyChain_Add)(int CommPort, char * SerialNumber);
typedef int(__stdcall *tFeasaCom_DaisyChain_Del)(int CommPort, char * SerialNumber);
typedef int(__stdcall *tFeasaCom_DaisyChain_Clear)(int CommPort);
typedef int(__stdcall *tFeasaCom_DaisyChain_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

// Comm handling functions
typedef int(__stdcall *tFeasaCom_EnumPorts)(void);
typedef int(__stdcall *tFeasaCom_IsConnected)(char * SerialNumber, char * Baudrate);
typedef int(__stdcall *tFeasaCom_SetResponseTimeout)(unsigned int Timeout);
typedef int(__stdcall *tFeasaCom_IsPortAvailable)(int CommPort);
typedef int(__stdcall *tFeasaCom_ListPortsDetected)(int * ListOfPortsDetected);
typedef int(__stdcall *tFeasaCom_ListPortsDetectedTxt)(char * ListOfPortsDetected, const char * Delimiter);
typedef long(__stdcall *tFeasaCom_GetBaudrate)(int CommPort);
typedef void(__stdcall *tFeasaCom_GetErrorDescription)(char * ErrorDescription);
typedef int(__stdcall *tFeasaCom_GetPortBySN)(char * SerialNumber);

// Binning
typedef int(__stdcall *tFeasaCom_BinningGetBinFromVECFile)(char * Path, float x, float y, char * ResultBinName);

// UserCal functions
typedef int(__stdcall *tFeasaCom_UserCal_ResetxyOffsets)(int CommPort, int Fiber, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_SetxyOffsets)(int CommPort, int Fiber, float xOffset, float yOffset, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_GetxyOffsets)(int CommPort, int Fiber, float * xOffset, float * yOffset);
typedef int(__stdcall *tFeasaCom_UserCal_AdjustxyOffsets)(int CommPort, int Fiber, float xRef, float yRef, int TOFLASH);

typedef int(__stdcall *tFeasaCom_UserCal_ResetWavelengthOffset)(int CommPort, int Fiber, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_GetWavelengthOffset)(int CommPort, int Fiber, int * WavelengthOffset);
typedef int(__stdcall *tFeasaCom_UserCal_SetWavelengthOffset)(int CommPort, int Fiber, int WavelengthOffset, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_AdjustWavelengthOffset)(int CommPort, int Fiber, int WavelengthRef, int TOFLASH);

typedef int(__stdcall *tFeasaCom_UserCal_ResetIntensity)(int CommPort, int Fiber, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_GetIntensityGain)(int CommPort, int Fiber, int * Gain);
typedef int(__stdcall *tFeasaCom_UserCal_SetIntensityGain)(int CommPort, int Fiber, int Gain, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_AdjustIntensity)(int CommPort, int Fiber, int IntensityRef, int isPWM, int CaptureRange, int TOFLASH);

typedef int(__stdcall *tFeasaCom_UserCal_ResetAbsInt)(int CommPort, int Fiber, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_GetAbsIntFactor)(int CommPort, int Fiber, double * Factor);
typedef int(__stdcall *tFeasaCom_UserCal_SetAbsIntFactor)(int CommPort, int Fiber, double Factor, int TOFLASH);
typedef int(__stdcall *tFeasaCom_UserCal_AdjustAbsInt)(int CommPort, int Fiber, double AbsIntRef, int TOFLASH);

typedef int(__stdcall *tFeasaCom_UserCal_ResetRGBAdj)(const int CommPort, const int Fiber);
typedef int(__stdcall *tFeasaCom_UserCal_TakeRGBCurrentValues)(const int CommPort, const int Fiber, const char Color);
typedef int(__stdcall *tFeasaCom_UserCal_AdjustRGB)(const int CommPort, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

#endif //FEASACOM_H