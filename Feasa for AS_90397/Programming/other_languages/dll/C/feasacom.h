#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

//------------------------------------------------------------------
// DLL TYPES
//------------------------------------------------------------------
// Basic Comm functions
typedef int (__stdcall *tFeasaCom_OpenSN)(char * SerialNumber, char * Baudrate);
typedef int (__stdcall *tFeasaCom_SendSN)(char * SerialNumber, char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_CloseSN)(char * SerialNumber);
typedef int (__stdcall *tFeasaCom_Open)(int CommPort, char * Baudrate);
typedef int (__stdcall *tFeasaCom_Send)(int CommPort, char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_Close)(int CommPort);

// Comm helper functions
typedef int (__stdcall *tFeasaCom_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_CaptureFromAll)(int * ReturnValues, int isPWM, int CaptureRange, int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_SendToAll)(int * ReturnValues, const char * Command, char ** Responses);
typedef int (__stdcall *tFeasaCom_SendToAll_NR)(int * ReturnValues, const const char * Command);
typedef int (__stdcall *tFeasaCom_Open_Multi)(int * ReturnValues, int * CommPorts, int nPorts, char * Baudrate);
typedef int (__stdcall *tFeasaCom_Close_Multi)(int * ReturnValues, int * CommPorts, int nPorts);
typedef int (__stdcall *tFeasaCom_Send_Multi)(int * ReturnValues, int * CommPorts, int nPorts, char ** Commands, char ** Responses);
typedef int (__stdcall *tFeasaCom_Send_Multi_NR)(int * ReturnValues, const int * CommPorts, const int nPorts, const char * Commands, const char CommandSeparator);
typedef int (__stdcall *tFeasaCom_OpenSN_Multi)(int * ReturnValues, char ** SerialNumbers, int nSerials, char * Baudrate);
typedef int (__stdcall *tFeasaCom_CloseSN_Multi)(int * ReturnValues, char ** SerialNumbers, int nSerials);
typedef int (__stdcall *tFeasaCom_SendSN_Multi)(int * ReturnValues, char ** SerialNumbers, int nSerials, char ** Commands, char ** Responses);
typedef int (__stdcall *tFeasaCom_CloseAll)(void);
typedef int (__stdcall *tFeasaCom_GetResponseByPort)(const int CommPort, char * ResponseText);

// Daisy-chain functions
typedef int (__stdcall *tFeasaCom_DaisyChain_Add)(int CommPort, char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Del)(int CommPort, char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Clear)(int CommPort);
typedef int (__stdcall *tFeasaCom_DaisyChain_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

// Comm handling functions
typedef int (__stdcall *tFeasaCom_EnumPorts)(void);
typedef int (__stdcall *tFeasaCom_IsConnected)(char * SerialNumber, char * Baudrate);
typedef int (__stdcall *tFeasaCom_AreConnected)(int * PortNumbers, char ** SerialNumbers, int nSerials, char * Baudrate);
typedef int (__stdcall *tFeasaCom_AreConnectedS)(int * PortNumbers, char * SerialNumbers, char * Baudrate);
typedef int (__stdcall *tFeasaCom_Detect)(int * CommPorts, char * Baudrate);
typedef int (__stdcall *tFeasaCom_DetectSN)(char ** SerialNumbers, char * Baudrate);
typedef int (__stdcall *tFeasaCom_IsPortAvailable)(int CommPort);
typedef int (__stdcall *tFeasaCom_ListPortsDetected)(int * ListOfPortsDetected);
typedef int (__stdcall *tFeasaCom_ListPortsDetectedTxt)(char * ListOfPortsDetected, const char * Delimiter);
typedef int (__stdcall *tFeasaCom_SetResponseTimeout)(unsigned int Timeout);
typedef long (__stdcall *tFeasaCom_GetBaudrate)(int CommPort);
typedef void (__stdcall *tFeasaCom_GetError_Description)(char * ErrorDescription);
typedef int (__stdcall *tFeasaCom_GetPortBySN)(char * SerialNumber);
typedef int (__stdcall *tFeasaCom_GetSNByPort)(char * SerialNumber, int CommPort);
typedef int (__stdcall *tFeasaCom_GetOpenedPorts)(int * CommPorts);
typedef int (__stdcall *tFeasaCom_GetOpenedPortsS)(char * CommPortsTxt, const char Delimiter);

// Binning
typedef int (__stdcall *tFeasaCom_Binning_GetBinFromVECFile)(char * Path, float x, float y, char * ResultBinName);

// UserCal functions
typedef int (__stdcall *tFeasaCom_UserCal_ResetxyOffsets)(int CommPort, int Fiber, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_SetxyOffsets)(int CommPort, int Fiber, float xOffset, float yOffset, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetxyOffsets)(int CommPort, int Fiber, float * xOffset, float * yOffset);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustxyOffsets)(int CommPort, int Fiber, float xRef, float yRef, int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetWavelengthOffset)(int CommPort, int Fiber, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetWavelengthOffset)(int CommPort, int Fiber, int * WavelengthOffset);
typedef int (__stdcall *tFeasaCom_UserCal_SetWavelengthOffset)(int CommPort, int Fiber, int WavelengthOffset, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustWavelengthOffset)(int CommPort, int Fiber, int WavelengthRef, int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetIntensity)(int CommPort, int Fiber, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetIntensityGain)(int CommPort, int Fiber, int * Gain);
typedef int (__stdcall *tFeasaCom_UserCal_SetIntensityGain)(int CommPort, int Fiber, int Gain, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustIntensity)(int CommPort, int Fiber, int IntensityRef, int isPWM, int CaptureRange, int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetAbsInt)(int CommPort, int Fiber, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetAbsIntFactor)(int CommPort, int Fiber, double * Factor);
typedef int (__stdcall *tFeasaCom_UserCal_SetAbsIntFactor)(int CommPort, int Fiber, double Factor, int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustAbsInt)(int CommPort, int Fiber, double AbsIntRef, int toFlash);
	
typedef int (__stdcall *tFeasaCom_UserCal_ResetRGBAdj)(const int CommPort, const int Fiber);
typedef int (__stdcall *tFeasaCom_UserCal_TakeRGBCurrentValues)(const int CommPort, const int Fiber, const char Color);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustRGB)(const int CommPort, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

//------------------------------------------------------------------
// EXTERN
//------------------------------------------------------------------

// Declare pointers to call functions
extern tFeasaCom_OpenSN FeasaCom_OpenSN;
extern tFeasaCom_CloseSN FeasaCom_CloseSN;
extern tFeasaCom_SendSN FeasaCom_SendSN;
extern tFeasaCom_Open FeasaCom_Open;
extern tFeasaCom_Close FeasaCom_Close;
extern tFeasaCom_Send FeasaCom_Send;

// Comm helper functions
extern tFeasaCom_Capture FeasaCom_Capture;
extern tFeasaCom_CaptureFromAll FeasaCom_CaptureFromAll;
extern tFeasaCom_SendToAll FeasaCom_SendToAll;
extern tFeasaCom_SendToAll_NR FeasaCom_SendToAll_NR;
extern tFeasaCom_Open_Multi FeasaCom_Open_Multi;
extern tFeasaCom_Close_Multi FeasaCom_Close_Multi;
extern tFeasaCom_Send_Multi FeasaCom_Send_Multi;
extern tFeasaCom_Send_Multi_NR FeasaCom_Send_Multi_NR;
extern tFeasaCom_OpenSN_Multi FeasaCom_OpenSN_Multi;
extern tFeasaCom_CloseSN_Multi FeasaCom_CloseSN_Multi;
extern tFeasaCom_SendSN_Multi FeasaCom_SendSN_Multi;
extern tFeasaCom_CloseAll FeasaCom_CloseAll;
extern tFeasaCom_GetResponseByPort FeasaCom_GetResponseByPort;

// Daisy-chain functions
extern tFeasaCom_DaisyChain_Add FeasaCom_DaisyChain_Add;
extern tFeasaCom_DaisyChain_Del FeasaCom_DaisyChain_Del;
extern tFeasaCom_DaisyChain_Clear FeasaCom_DaisyChain_Clear;
extern tFeasaCom_DaisyChain_Capture FeasaCom_DaisyChain_Capture;

// Comm handling functions
extern tFeasaCom_EnumPorts FeasaCom_EnumPorts;
extern tFeasaCom_IsConnected FeasaCom_IsConnected;
extern tFeasaCom_AreConnected FeasaCom_AreConnected;
extern tFeasaCom_AreConnectedS FeasaCom_AreConnectedS;
extern tFeasaCom_Detect FeasaCom_Detect;
extern tFeasaCom_DetectSN FeasaCom_DetectSN;
extern tFeasaCom_IsPortAvailable FeasaCom_IsPortAvailable;
extern tFeasaCom_ListPortsDetected FeasaCom_ListPortsDetected;
extern tFeasaCom_ListPortsDetectedTxt FeasaCom_ListPortsDetectedTxt;
extern tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;
extern tFeasaCom_GetBaudrate FeasaCom_GetBaudrate;
extern tFeasaCom_GetError_Description FeasaCom_GetError_Description;
extern tFeasaCom_GetPortBySN FeasaCom_GetPortBySN;
extern tFeasaCom_GetSNByPort FeasaCom_GetSNByPort;
extern tFeasaCom_GetOpenedPorts FeasaCom_GetOpenedPorts;
extern tFeasaCom_GetOpenedPortsS FeasaCom_GetOpenedPortsS;

// Binning
extern tFeasaCom_Binning_GetBinFromVECFile FeasaCom_Binning_GetBinFromVECFile;

// UserCal functions
extern tFeasaCom_UserCal_ResetxyOffsets FeasaCom_UserCal_ResetxyOffsets;
extern tFeasaCom_UserCal_SetxyOffsets FeasaCom_UserCal_SetxyOffsets;
extern tFeasaCom_UserCal_GetxyOffsets FeasaCom_UserCal_GetxyOffsets;
extern tFeasaCom_UserCal_AdjustxyOffsets FeasaCom_UserCal_AdjustxyOffsets;

extern tFeasaCom_UserCal_ResetWavelengthOffset FeasaCom_UserCal_ResetWavelengthOffset;
extern tFeasaCom_UserCal_GetWavelengthOffset FeasaCom_UserCal_GetWavelengthOffset;
extern tFeasaCom_UserCal_SetWavelengthOffset FeasaCom_UserCal_SetWavelengthOffset;
extern tFeasaCom_UserCal_AdjustWavelengthOffset FeasaCom_UserCal_AdjustWavelengthOffset;

extern tFeasaCom_UserCal_ResetIntensity FeasaCom_UserCal_ResetIntensity;
extern tFeasaCom_UserCal_GetIntensityGain FeasaCom_UserCal_GetIntensityGain;
extern tFeasaCom_UserCal_SetIntensityGain FeasaCom_UserCal_SetIntensityGain;
extern tFeasaCom_UserCal_AdjustIntensity FeasaCom_UserCal_AdjustIntensity;

extern tFeasaCom_UserCal_ResetAbsInt FeasaCom_UserCal_ResetAbsInt;
extern tFeasaCom_UserCal_GetAbsIntFactor FeasaCom_UserCal_GetAbsIntFactor;
extern tFeasaCom_UserCal_SetAbsIntFactor FeasaCom_UserCal_SetAbsIntFactor;
extern tFeasaCom_UserCal_AdjustAbsInt FeasaCom_UserCal_AdjustAbsInt;

extern tFeasaCom_UserCal_ResetRGBAdj FeasaCom_UserCal_ResetRGBAdj;
extern tFeasaCom_UserCal_TakeRGBCurrentValues FeasaCom_UserCal_TakeRGBCurrentValues;
extern tFeasaCom_UserCal_AdjustRGB FeasaCom_UserCal_AdjustRGB;

int FeasaCom_Load(int is64bit, char * PathToDLL);
void FeasaCom_UnLoad();
void FormatDecimal(char * buffer);

#endif //FEASACOM_H