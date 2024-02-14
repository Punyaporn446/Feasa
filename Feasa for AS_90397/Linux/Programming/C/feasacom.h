#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Dynamic Library
********************************************************/

//--------------------------------------------------------------
// SO LIBRARY TYPES
//--------------------------------------------------------------

typedef void FeasaCom_GetLibraryVersion(char * Version);

// Basic Comm functions
typedef int (*tFeasaCom_Open)(const char * DevPath, const int Baudrate);
typedef int (*tFeasaCom_Close)(const char * DevPath);
typedef int (*tFeasaCom_Send)(const char * DevPath, const char * Command, char * ResponseText);
typedef int (*tFeasaCom_OpenSN)(const char * SerialNumber, const int Baudrate);
typedef int (*tFeasaCom_CloseSN)(const char * SerialNumber);
typedef int (*tFeasaCom_SendSN)(const char * SerialNumber, const char * Command, char * ResponseText);

// Comm helper functions
typedef int (*tFeasaCom_SendToAll)(int * ReturnValues, const char * Command, char ** Responses);
typedef int (*tFeasaCom_SendToAll_NR)(int * ReturnValues, const char * Command);
typedef int (*tFeasaCom_Open_Multi)(int * ReturnValues, const char ** DevPaths, const int nPorts, const int Baudrate);
typedef int (*tFeasaCom_Close_Multi)(int * ReturnValues, const char ** DevPaths, const int nPorts);
typedef int (*tFeasaCom_Send_Multi)(int * ReturnValues, const char ** DevPaths, const int nPorts, const char ** Commands, char ** Responses);
typedef int (*tFeasaCom_Send_Multi_NR)(int * ReturnValues, const char * DevPaths, const int nPorts, const char * Commands, const char CommandSeparator);
typedef int (*tFeasaCom_OpenSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const int Baudrate);
typedef int (*tFeasaCom_CloseSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials);
typedef int (*tFeasaCom_SendSN_Multi)(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char ** Commands, char ** Responses);
typedef int (*tFeasaCom_CloseAll)(void);
typedef int (*tFeasaCom_GetResponseByPort)(const char * DevPath, char * ResponseText);
typedef int (*tFeasaCom_GetOpenedPorts)(char ** DevPaths);
typedef int (*tFeasaCom_GetOpenedPortsS)(char * DevPaths, const char Delimiter);
typedef int (*tFeasaCom_OpenProject)(const char * Path);
typedef int (*tFeasaCom_CloseProject)(void);
typedef int (*tFeasaCom_SendByID)(const char * DeviceID, const char * Command, char * ResponseText);

// Test functions
typedef int (*tFeasaCom_Capture)(const char * DevPath, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (*tFeasaCom_CaptureFromAll)(int * ReturnValues, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (*tFeasaCom_SpectrometerCapture)(const char * DevPath, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (*tFeasaCom_SpectrometerDark)(const char * DevPath, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (*tFeasaCom_CaptureFromAllSpectrometers)(int * ReturnValues, const int isPWM, const int UseCustomExposure, const float ExposureTime);
typedef int (*tFeasaCom_Sequence_Setup)(const char * DevPath, const int StartDelay, const int CaptureTime, const int TimeBetweenCaptures, const int SampleCount, const int toFlash);
typedef int (*tFeasaCom_Sequence_Capture)(const char * DevPath, const int Fiber);
typedef int (*tFeasaCom_Sequence_ReadIntensity)(const char * DevPath, const int Fiber, int * IntensityValues);
typedef int (*tFeasaCom_Sequence_ReadxyI)(const char * DevPath, const int Fiber, float * xValues, float * yValues, int * IntensityValues);
typedef int (*tFeasaCom_Sequence_ReadHSI)(const char * DevPath, const int Fiber, float * HueValues, int * SaturationValues, int * IntensityValues);
typedef int (*tFeasaCom_Sequence_ReadRGBI)(const char * DevPath, const int Fiber, unsigned char * RedValues, unsigned char * GreenValues, unsigned char * BlueValues, int * IntensityValues);
typedef int (*tFeasaCom_Sequence_ReadCCT)(const char * DevPath, const int Fiber, int * CCTValues, float * deltauvValues);
typedef int (*tFeasaCom_Sequence_ReadWavelength)(const char * DevPath, const int Fiber, int * WavelengthValues);
typedef int (*tFeasaCom_Sequence_GetPattern)(const char * DevPath, const int * IntensityValues, int * StatusCount, int * PatternTimes, int * PatternIntensities);
typedef int (*tFeasaCom_Sequence_GetSweepingPattern)(const char * DevPath, const int LEDCount, const int isOffToOn, int * LowTimes, int * HighTimes, int * IntensityValues);
typedef int (*tFeasaCom_Sequence_GetFrequency)(const char * DevPath, const int * IntensityValues, float * Frequency, float * DC, int * CycleCount);
typedef int (*tFeasaCom_Sequence_FindTestSettings)(const char * DevPath, const int TotalLEDCount, const int FiberToTest, const int SignalSpeed, const int BlinkingSpeed, const int MinCycleCount, const int TimeResolutionIsImportant, int * CaptureTime, int * WaitTime, int * SampleCount);
typedef int (*tFeasaCom_Sequence_SetPatternThresholdHigh)(const char * DevPath, const int Intensity);
typedef int (*tFeasaCom_Sequence_SetPatternThresholdLow)(const char * DevPath, const int Intensity);

// Daisy-chain functions
typedef int (*tFeasaCom_DaisyChain_Add)(const char * DevPath, const char * SerialNumber);
typedef int (*tFeasaCom_DaisyChain_Del)(const char * DevPath, const char * SerialNumber);
typedef int (*tFeasaCom_DaisyChain_Clear)(const char * DevPath);
typedef int (*tFeasaCom_DaisyChain_Send)(const char * DevPath, const char * SerialNumber, const char * Command, char * ResponseText);
typedef int (*tFeasaCom_DaisyChain_Capture)(const char * DevPath, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
typedef int (*tFeasaCom_DaisyChain_SpectrometerCapture)(const char * DevPath, const int UsePresetExposure, const int UseCustomExposure, const float ExposureTime);
typedef int (*tFeasaCom_DaisyChain_SpectrometerDark)(const char * DevPath, const int isPWM, const int UsePresetExposure, const float ExposureTime);

// External Trigger functions
typedef int (*tFeasaCom_ExternalTrigger_Listen)(const char * DevPath);
typedef int (*tFeasaCom_ExternalTrigger_Abort)(const char * DevPath);
typedef int (*tFeasaCom_ExternalTrigger_isFinished)(const char * DevPath);
typedef int (*tFeasaCom_ExternalTrigger_Enable)(const char * DevPath, const int CaptureRange, const int isPWM, const char * OutputType, const int PreDelay, const int PostDelay, const int toFlash);
typedef int (*tFeasaCom_ExternalTrigger_Disable)(const char * DevPath, const int toFlash);

// Comm handling functions
typedef int (*tFeasaCom_EnumPorts)(void);
typedef int (*tFeasaCom_IsConnected)(char * DevPath, const char * SerialNumber, const int Baudrate);
typedef int (*tFeasaCom_AreConnected)(char ** DevPaths, const char ** SerialNumbers, const int nSerials, const int Baudrate);
typedef int (*tFeasaCom_AreConnectedS)(char * DevPaths, const char * SerialNumbers, const int Baudrate);
typedef int (*tFeasaCom_Detect)(char ** DevPaths, const int Baudrate);
typedef int (*tFeasaCom_DetectS)(char * DevPaths, const char Delimiter, const int Baudrate);
typedef int (*tFeasaCom_DetectSN)(char ** SerialNumbers, const int Baudrate);
typedef void (*tFeasaCom_AddDetectionFilter)(const char * Filter);
typedef void (*tFeasaCom_ClearDetectionFilters)(void);
typedef int (*tFeasaCom_IsPortAvailable)(const char * DevPath);
typedef int (*tFeasaCom_ListPortsDetected)(char ** ListOfPortsDetected);
typedef int (*tFeasaCom_ListPortsDetectedTxt)(char * ListOfPortsDetected, const char * Delimiter);
typedef int (*tFeasaCom_SetResponseTimeout)(const unsigned int Timeout);
typedef int (*tFeasaCom_SetResponseTimeoutAuto)(const char * DevPath, const int status);
typedef long (*tFeasaCom_GetBaudrate)(const char * DevPath);
typedef long (*tFeasaCom_GetDeviceType)(const char * DevPath, char * DeviceType);
typedef void (*tFeasaCom_GetError_Description)(char * ErrorDescription);
typedef void (*tFeasaCom_GetError_DescriptionByPort)(const char * DevPath, char * ErrorDescription);
typedef void (*tFeasaCom_GetError_DescriptionBySN)(const char * SerialNumber, char * ErrorDescription);
typedef int (*tFeasaCom_GetPortBySN)(char * DevPath, const char * SerialNumber);
typedef int (*tFeasaCom_GetSNByPort)(char * SerialNumber, const char * DevPath);
typedef void (*tFeasaCom_GetPortByID)(const char * DeviceID, char * DevPath);

// Binning
typedef int (*tFeasaCom_Binning_GetBinFromVECFile)(const char * Path, const float x, const float y, char * ResultBinName);

// UserCal functions
typedef int (*tFeasaCom_UserCal_ResetxyOffsets)(const char * DevPath, const int Fiber, const int toFlash);
typedef int (*tFeasaCom_UserCal_SetxyOffsets)(const char * DevPath, const int Fiber, const float xOffset, const float yOffset, const int toFlash);
typedef int (*tFeasaCom_UserCal_GetxyOffsets)(const char * DevPath, const int Fiber, float * xOffset, float * yOffset);
typedef int (*tFeasaCom_UserCal_AdjustxyOffsets)(const char * DevPath, const int Fiber, const float xRef, const float yRef, const int toFlash);

typedef int (*tFeasaCom_UserCal_ResetWavelengthOffset)(const char * DevPath, const int Fiber, const int toFlash);
typedef int (*tFeasaCom_UserCal_GetWavelengthOffset)(const char * DevPath, const int Fiber, int * WavelengthOffset);
typedef int (*tFeasaCom_UserCal_SetWavelengthOffset)(const char * DevPath, const int Fiber, const int WavelengthOffset, const int toFlash);
typedef int (*tFeasaCom_UserCal_AdjustWavelengthOffset)(const char * DevPath, const int Fiber, const int WavelengthRef, const int toFlash);

typedef int (*tFeasaCom_UserCal_ResetIntensity)(const char * DevPath, const int Fiber, const int toFlash);
typedef int (*tFeasaCom_UserCal_GetIntensityGain)(const char * DevPath, const int Fiber, int * Gain);
typedef int (*tFeasaCom_UserCal_SetIntensityGain)(const char * DevPath, const int Fiber, const int Gain, const int toFlash);
typedef int (*tFeasaCom_UserCal_AdjustIntensity)(const char * DevPath, const int Fiber, const int IntensityRef, const int isPWM, const int CaptureRange, const int toFlash);

typedef int (*tFeasaCom_UserCal_ResetAbsInt)(const char * DevPath, const int Fiber, const int toFlash);
typedef int (*tFeasaCom_UserCal_GetAbsIntFactor)(const char * DevPath, const int Fiber, double * Factor);
typedef int (*tFeasaCom_UserCal_SetAbsIntFactor)(const char * DevPath, const int Fiber, const double Factor, const int toFlash);
typedef int (*tFeasaCom_UserCal_AdjustAbsInt)(const char * DevPath, const int Fiber, const double AbsIntRef, const int toFlash);

typedef int (*tFeasaCom_UserCal_ResetRGBAdj)(const char * DevPath, const int Fiber);
typedef int (*tFeasaCom_UserCal_TakeRGBCurrentValues)(const char * DevPath, const int Fiber, const char Color);
typedef int (*tFeasaCom_UserCal_AdjustRGB)(const char * DevPath, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

//--------------------------------------------------------------
// EXTERN
//--------------------------------------------------------------

extern tFeasaCom_GetLibraryVersion FeasaCom_GetLibraryVersion;

// Declare pointers to call functions
extern tFeasaCom_OpenSN FeasaCom_OpenSN;
extern tFeasaCom_SendSN FeasaCom_SendSN;
extern tFeasaCom_CloseSN FeasaCom_CloseSN;
extern tFeasaCom_Open FeasaCom_Open;
extern tFeasaCom_Send FeasaCom_Send;
extern tFeasaCom_Close FeasaCom_Close;

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
extern tFeasaCom_GetOpenedPorts FeasaCom_GetOpenedPorts;
extern tFeasaCom_GetOpenedPortsS FeasaCom_GetOpenedPortsS;
extern tFeasaCom_OpenProject FeasaCom_OpenProject;
extern tFeasaCom_CloseProject FeasaCom_CloseProject;
extern tFeasaCom_SendByID FeasaCom_SendByID;

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

//External Trigger functions
extern tFeasaCom_ExternalTrigger_Listen FeasaCom_ExternalTrigger_Listen;
extern tFeasaCom_ExternalTrigger_Abort FeasaCom_ExternalTrigger_Abort;
extern tFeasaCom_ExternalTrigger_isFinished FeasaCom_ExternalTrigger_isFinished;
extern tFeasaCom_ExternalTrigger_Enable FeasaCom_ExternalTrigger_Enable;
extern tFeasaCom_ExternalTrigger_Disable FeasaCom_ExternalTrigger_Disable;

// Comm handling functions
extern tFeasaCom_EnumPorts FeasaCom_EnumPorts;
extern tFeasaCom_IsConnected FeasaCom_IsConnected;
extern tFeasaCom_AreConnected FeasaCom_AreConnected;
extern tFeasaCom_AreConnectedS FeasaCom_AreConnectedS;
extern tFeasaCom_Detect FeasaCom_Detect;
extern tFeasaCom_DetectS FeasaCom_DetectS;
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

int FeasaCom_Load(const char * PathToLibrary);
void FeasaCom_UnLoad();
void FormatDecimal(char * buffer);
void FreeArrayOfStrings(char ** mArrayOfStrings, const int Size);

#endif //FEASACOM_H
