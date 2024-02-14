#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Communications Library
*
*  TO USE ALONG WITH DYNAMIC DLL LOADING
*
********************************************************/

//------------------------------------------------------------------
// DLL TYPES
//------------------------------------------------------------------
typedef void (__stdcall *tFeasaCom_GetDLLVersion)(char * Version);

// Basic Comm functions
typedef int (__stdcall *tFeasaCom_Open)(const int CommPort, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_Send)(const int CommPort, const char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_Close)(const int CommPort);
typedef int (__stdcall *tFeasaCom_OpenSN)(const char * SerialNumber, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_SendSN)(const char * SerialNumber, const char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_CloseSN)(const char * SerialNumber);

// Comm helper functions
typedef int (__stdcall *tFeasaCom_SendToAll)(int * ReturnValues, const const char * Command, char ** Responses);
typedef int (__stdcall *tFeasaCom_SendToAll_NR)(int * ReturnValues, const const char * Command);
typedef int (__stdcall *tFeasaCom_Open_Multi)(int * ReturnValues, const int * CommPorts, const int nPorts, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_Close_Multi)(int * ReturnValues, const int * CommPorts, const int nPorts);
typedef int (__stdcall *tFeasaCom_Send_Multi)(int * ReturnValues, const int * CommPorts, const int nPorts, const char ** Commands, char ** Responses);
typedef int (__stdcall *tFeasaCom_Send_Multi_NR)(int * ReturnValues, const int * CommPorts, const int nPorts, const char * Commands, const char CommandSeparator);
typedef int (__stdcall *tFeasaCom_OpenSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_CloseSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials);
typedef int (__stdcall *tFeasaCom_SendSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char ** Commands, char ** Responses);
typedef int (__stdcall *tFeasaCom_CloseAll)(void);
typedef int (__stdcall *tFeasaCom_GetResponseByPort)(const int CommPort, char * ResponseText);
typedef int (__stdcall *tFeasaCom_GetOpenedPorts)(int * CommPorts);
typedef int (__stdcall *tFeasaCom_GetOpenedPortsS)(char * CommPortsTxt, const char Delimiter);

// Test functions
typedef int (__stdcall *tFeasaCom_Capture)(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_CaptureFromAll)(int * ReturnValues, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_SpectrometerCapture)(const int CommPort, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (__stdcall *tFeasaCom_SpectrometerDark)(const int CommPort, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (__stdcall *tFeasaCom_CaptureFromAllSpectrometers)(int * ReturnValues, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (__stdcall *tFeasaCom_Sequence_Setup)(const int CommPort, const int StartDelay, const int CaptureTime, const int TimeBetweenCaptures, const int SampleCount, const int toFlash);
typedef int (__stdcall *tFeasaCom_Sequence_Capture)(const int CommPort, const int Fiber);
typedef int (__stdcall *tFeasaCom_Sequence_ReadIntensity)(const int CommPort, const int Fiber, int * IntensityValues);
typedef int (__stdcall *tFeasaCom_Sequence_ReadxyI)(const int CommPort, const int Fiber, float * xValues, float * yValues, int * IntensityValues);
typedef int (__stdcall *tFeasaCom_Sequence_ReadHSI)(const int CommPort, const int Fiber, float * HueValues, int * SaturationValues, int * IntensityValues);
typedef int (__stdcall *tFeasaCom_Sequence_ReadRGBI)(const int CommPort, const int Fiber, unsigned char * RedValues, unsigned char * GreenValues, unsigned char * BlueValues, int * IntensityValues);
typedef int (__stdcall *tFeasaCom_Sequence_ReadCCT)(const int CommPort, const int Fiber, int * CCTValues, float * deltauvValues);
typedef int (__stdcall *tFeasaCom_Sequence_ReadWavelength)(const int CommPort, const int Fiber, int * WavelengthValues);
typedef int (__stdcall *tFeasaCom_Sequence_GetPattern)(const int CommPort, const int * IntensityValues, int * StatusCount, int * PatternTimes, int * PatternIntensities);
typedef int (__stdcall *tFeasaCom_Sequence_GetSweepingPattern)(const int CommPort, const int LEDCount, const int isOffToOn, int * LowTimes, int * HighTimes, int * IntensityValues);
typedef int (__stdcall *tFeasaCom_Sequence_GetFrequency)(const int CommPort, const int * IntensityValues, float * Frequency, float * DC, int * CycleCount);
typedef int (__stdcall *tFeasaCom_Sequence_FindTestSettings)(const int CommPort, const int TotalLEDCount, const int FiberToTest, const int SignalSpeed, const int BlinkingSpeed, const int MinCycleCount, const int TimeResolutionIsImportant, int * CaptureTime, int * WaitTime, int * SampleCount);
typedef int (__stdcall *tFeasaCom_Sequence_SetPatternThresholdHigh)(const int CommPort, const int Intensity);
typedef int (__stdcall *tFeasaCom_Sequence_SetPatternThresholdLow)(const int CommPort, const int Intensity);

// Daisy-chain functions
typedef int (__stdcall *tFeasaCom_DaisyChain_Add)(const int CommPort, const char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Del)(const int CommPort, const char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Clear)(const int CommPort);
typedef int (__stdcall *tFeasaCom_DaisyChain_Send)(int CommPort, const char * SerialNumber, const char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_DaisyChain_Capture)(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_DaisyChain_SpectrometerCapture)(const int CommPort, const int isPWM, const int UsePresetExposure, const float ExposureTime);
typedef int (__stdcall *tFeasaCom_DaisyChain_SpectrometerDark)(const int CommPort, const int isPWMt, const int UsePresetExposure, const float ExposureTime);

// External Trigger functions
typedef int (__stdcall *tFeasaCom_ExternalTrigger_Listen)(const int CommPort);
typedef int (__stdcall *tFeasaCom_ExternalTrigger_Abort)(const int CommPort);
typedef int (__stdcall *tFeasaCom_ExternalTrigger_isFinished)(const int CommPort);

// Comm handling functions
typedef int (__stdcall *tFeasaCom_EnumPorts)(void);
typedef void (__stdcall *tFeasaCom_EnumPorts_Filter)(const int USB, const int RS232, const int Bluetooth);
typedef int (__stdcall *tFeasaCom_IsConnected)(const char * SerialNumber, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_AreConnected)(int * PortNumbers, const char ** SerialNumbers, const int nSerials, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_AreConnectedS)(int * PortNumbers, const char * SerialNumbers, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_Detect)(int * CommPorts, const char * Baudrate);
typedef int (__stdcall *tFeasaCom_DetectSN)(char ** SerialNumbers, const char * Baudrate);
typedef void (__stdcall *tFeasaCom_AddDetectionFilter)(const char * Filter);
typedef void (__stdcall *tFeasaCom_ClearDetectionFilters)(void);
typedef int (__stdcall *tFeasaCom_IsPortAvailable)(const int CommPort);
typedef int (__stdcall *tFeasaCom_ListPortsDetected)(int * ListOfPortsDetected);
typedef int (__stdcall *tFeasaCom_ListPortsDetectedTxt)(char * ListOfPortsDetected, const char * Delimiter);
typedef int (__stdcall *tFeasaCom_SetResponseTimeout)(const unsigned int Timeout);
typedef int (__stdcall *TFeasaCom_SetResponseTimeoutAuto)(const int CommPort, const int status);
typedef long (__stdcall *tFeasaCom_GetBaudrate)(const int CommPort);
typedef int (__stdcall *tFeasaCom_GetDeviceType)(const int CommPort, char * DeviceType);
typedef void (__stdcall *tFeasaCom_GetError_Description)(char * ErrorDescription);
typedef void (__stdcall *tFeasaCom_GetError_DescriptionByPort)(const int CommPort, char * ErrorDescription);
typedef void (__stdcall *tFeasaCom_GetError_DescriptionBySN)(const char * SerialNumber, char * ErrorDescription);
typedef int (__stdcall *tFeasaCom_GetPortBySN)(const char * SerialNumber);
typedef int (__stdcall *tFeasaCom_GetSNByPort)(char * SerialNumber, const int CommPort);
typedef int (__stdcall *tFeasaCom_GetPortByID)(const char * DeviceID);
typedef int (__stdcall *tFeasaCom_OpenProject)(const char * Path);
typedef int (__stdcall *tFeasaCom_CloseProject)(void);
typedef int (__stdcall *tFeasaCom_SendByID)(const char * DeviceID, const char * Command, char * ResponseText);

// Binning
typedef int (__stdcall *tFeasaCom_Binning_GetBinFromVECFile)(const char * Path, const float x, const float y, char * ResultBinName);

// UserCal functions
typedef int (__stdcall *tFeasaCom_UserCal_ResetxyOffsets)(const int CommPort, const int Fiber, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_SetxyOffsets)(const int CommPort, const int Fiber, const float xOffset, const float yOffset, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetxyOffsets)(const int CommPort, const int Fiber, float * xOffset, float * yOffset);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustxyOffsets)(const int CommPort, const int Fiber, const float xRef, const float yRef, const int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetWavelengthOffset)(const int CommPort, const int Fiber, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetWavelengthOffset)(const int CommPort, const int Fiber, int * WavelengthOffset);
typedef int (__stdcall *tFeasaCom_UserCal_SetWavelengthOffset)(const int CommPort, const int Fiber, int const WavelengthOffset, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustWavelengthOffset)(const int CommPort, const int Fiber, int const WavelengthRef, const int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetIntensity)(const int CommPort, const int Fiber, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetIntensityGain)(const int CommPort, const int Fiber, int * Gain);
typedef int (__stdcall *tFeasaCom_UserCal_SetIntensityGain)(const int CommPort, const int Fiber, int const Gain, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustIntensity)(const int CommPort, const int Fiber, int const IntensityRef, const int isPWM, const int CaptureRange, const int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetAbsInt)(const int CommPort, const int Fiber, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_GetAbsIntFactor)(const int CommPort, const int Fiber, double * Factor);
typedef int (__stdcall *tFeasaCom_UserCal_SetAbsIntFactor)(const int CommPort, const int Fiber, double const Factor, const int toFlash);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustAbsInt)(const int CommPort, const int Fiber, double const AbsIntRef, const int toFlash);

typedef int (__stdcall *tFeasaCom_UserCal_ResetRGBAdj)(const int CommPort, const int Fiber);
typedef int (__stdcall *tFeasaCom_UserCal_TakeRGBCurrentValues)(const int CommPort, const int Fiber, const char Color);
typedef int (__stdcall *tFeasaCom_UserCal_AdjustRGB)(const int CommPort, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

//------------------------------------------------------------------
// EXTERN
//------------------------------------------------------------------

extern tFeasaCom_GetDLLVersion FeasaCom_GetDLLVersion;

// Declare pointers to call functions
extern tFeasaCom_OpenSN FeasaCom_OpenSN;
extern tFeasaCom_CloseSN FeasaCom_CloseSN;
extern tFeasaCom_SendSN FeasaCom_SendSN;
extern tFeasaCom_Open FeasaCom_Open;
extern tFeasaCom_Close FeasaCom_Close;
extern tFeasaCom_Send FeasaCom_Send;

// Comm helper functions
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
extern tFeasaCom_GetOpenedPorts FeasaCom_GetOpenedPorts;
extern tFeasaCom_GetOpenedPortsS FeasaCom_GetOpenedPortsS;

// Test functions
extern tFeasaCom_Capture FeasaCom_Capture;
extern tFeasaCom_CaptureFromAll FeasaCom_CaptureFromAll;
extern tFeasaCom_SpectrometerCapture FeasaCom_SpectrometerCapture;
extern tFeasaCom_SpectrometerDark FeasaCom_SpectrometerDark;
extern tFeasaCom_CaptureFromAllSpectrometers FeasaCom_CaptureFromAllSpectrometers;
extern tFeasaCom_Sequence_Setup FeasaCom_Sequence_Setup;
extern tFeasaCom_Sequence_Capture FeasaCom_Sequence_Capture;
extern tFeasaCom_Sequence_ReadIntensity FeasaCom_Sequence_ReadIntensity;
extern tFeasaCom_Sequence_ReadxyI FeasaCom_Sequence_ReadxyI;
extern tFeasaCom_Sequence_ReadHSI FeasaCom_Sequence_ReadHSI;
extern tFeasaCom_Sequence_ReadRGBI FeasaCom_Sequence_ReadRGBI;
extern tFeasaCom_Sequence_ReadCCT FeasaCom_Sequence_ReadCCT;
extern tFeasaCom_Sequence_ReadWavelength FeasaCom_Sequence_ReadWavelength;
extern tFeasaCom_Sequence_GetPattern FeasaCom_Sequence_GetPattern;
extern tFeasaCom_Sequence_GetSweepingPattern FeasaCom_Sequence_GetSweepingPattern;
extern tFeasaCom_Sequence_GetFrequency FeasaCom_Sequence_GetFrequency;
extern tFeasaCom_Sequence_FindTestSettings FeasaCom_Sequence_FindTestSettings;
extern tFeasaCom_Sequence_SetPatternThresholdHigh FeasaCom_Sequence_SetPatternThresholdHigh;
extern tFeasaCom_Sequence_SetPatternThresholdLow FeasaCom_Sequence_SetPatternThresholdLow;

// Daisy-chain functions
extern tFeasaCom_DaisyChain_Add FeasaCom_DaisyChain_Add;
extern tFeasaCom_DaisyChain_Del FeasaCom_DaisyChain_Del;
extern tFeasaCom_DaisyChain_Clear FeasaCom_DaisyChain_Clear;
extern tFeasaCom_DaisyChain_Send FeasaCom_DaisyChain_Send;
extern tFeasaCom_DaisyChain_Capture FeasaCom_DaisyChain_Capture;
extern tFeasaCom_DaisyChain_SpectrometerCapture FeasaCom_DaisyChain_SpectrometerCapture;
extern tFeasaCom_DaisyChain_SpectrometerDark FeasaCom_DaisyChain_SpectrometerDark;

// External Trigger functions
extern tFeasaCom_ExternalTrigger_Listen FeasaCom_ExternalTrigger_Listen;
extern tFeasaCom_ExternalTrigger_Abort FeasaCom_ExternalTrigger_Abort;
extern tFeasaCom_ExternalTrigger_isFinished FeasaCom_ExternalTrigger_isFinished;

// Comm handling functions
extern tFeasaCom_EnumPorts FeasaCom_EnumPorts;
extern tFeasaCom_EnumPorts_Filter FeasaCom_EnumPorts_Filter;
extern tFeasaCom_IsConnected FeasaCom_IsConnected;
extern tFeasaCom_AreConnected FeasaCom_AreConnected;
extern tFeasaCom_AreConnectedS FeasaCom_AreConnectedS;
extern tFeasaCom_Detect FeasaCom_Detect;
extern tFeasaCom_DetectSN FeasaCom_DetectSN;
extern tFeasaCom_AddDetectionFilter FeasaCom_AddDetectionFilter;
extern tFeasaCom_ClearDetectionFilters FeasaCom_ClearDetectionFilters;
extern tFeasaCom_IsPortAvailable FeasaCom_IsPortAvailable;
extern tFeasaCom_ListPortsDetected FeasaCom_ListPortsDetected;
extern tFeasaCom_ListPortsDetectedTxt FeasaCom_ListPortsDetectedTxt;
extern tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;
extern tFeasaCom_SetResponseTimeoutAuto FeasaCom_SetResponseTimeoutAuto;
extern tFeasaCom_GetBaudrate FeasaCom_GetBaudrate;
extern tFeasaCom_GetDeviceType FeasaCom_GetDeviceType;
extern tFeasaCom_GetError_Description FeasaCom_GetError_Description;
extern tFeasaCom_GetError_DescriptionByPort FeasaCom_GetError_DescriptionByPort;
extern tFeasaCom_GetError_DescriptionBySN FeasaCom_GetError_DescriptionBySN;
extern tFeasaCom_GetPortBySN FeasaCom_GetPortBySN;
extern tFeasaCom_GetSNByPort FeasaCom_GetSNByPort;
extern tFeasaCom_GetPortByID FeasaCom_GetPortByID;
extern tFeasaCom_OpenProject FeasaCom_OpenProject;
extern tFeasaCom_CloseProject FeasaCom_CloseProject;
extern tFeasaCom_SendByID FeasaCom_SendByID;

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