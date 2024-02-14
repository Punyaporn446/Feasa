#ifndef FEASACOM_H
#define FEASACOM_H



/********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Communications Library
*
*  TO USE ALONG WITH LIB FILES
*
********************************************************/

void FeasaCom_GetDLLVersion(char * Version);

// Basic Comm functions
int FeasaCom_Open(const int CommPort, const char * Baudrate);
int FeasaCom_Send(const int CommPort, const char * Command, char * ResponseText);
int FeasaCom_Close(const int CommPort);
int FeasaCom_OpenSN(const char * SerialNumber, const char * Baudrate);
int FeasaCom_SendSN(const char * SerialNumber, const char * Command, char * ResponseText);
int FeasaCom_CloseSN(const char * SerialNumber);

// Comm helper functions
int FeasaCom_SendToAll(int * ReturnValues, const const char * Command, char ** Responses);
int FeasaCom_SendToAll_NR(int * ReturnValues, const const char * Command);
int FeasaCom_Open_Multi(int * ReturnValues, const int * CommPorts, const int nPorts, const char * Baudrate);
int FeasaCom_Close_Multi(int * ReturnValues, const int * CommPorts, const int nPorts);
int FeasaCom_Send_Multi(int * ReturnValues, const int * CommPorts, const int nPorts, const char ** Commands, char ** Responses);
int FeasaCom_Send_Multi_NR(int * ReturnValues, const int * CommPorts, const int nPorts, const char * Commands, const char CommandSeparator);
int FeasaCom_OpenSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char * Baudrate);
int FeasaCom_CloseSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials);
int FeasaCom_SendSN_Multi(int * ReturnValues, const char ** SerialNumbers, const int nSerials, const char ** Commands, char ** Responses);
int FeasaCom_CloseAll(void);
int FeasaCom_GetResponseByPort(const int CommPort, char * ResponseText);
int FeasaCom_GetOpenedPorts(int * CommPorts);
int FeasaCom_GetOpenedPortsS(char * CommPortsTxt, const char Delimiter);

// Test functions
int FeasaCom_Capture(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
int FeasaCom_CaptureFromAll(int * ReturnValues, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
int FeasaCom_SpectrometerCapture(const int CommPort, const int isPWM, const int UseCustomExposure, const float ExposureTime);
int FeasaCom_SpectrometerDark(const int CommPort, const int isPWM, const int UseCustomExposure, const float ExposureTime);
int FeasaCom_CaptureFromAllSpectrometers(int * ReturnValues, const int isPWM, const int UseCustomExposure, const float ExposureTime);
int FeasaCom_Sequence_Setup(const int CommPort, const int StartDelay, const int CaptureTime, const int TimeBetweenCaptures, const int SampleCount, const int toFlash);
int FeasaCom_Sequence_Capture(const int CommPort, const int Fiber);
int FeasaCom_Sequence_ReadIntensity(const int CommPort, const int Fiber, int * IntensityValues);
int FeasaCom_Sequence_ReadxyI(const int CommPort, const int Fiber, float * xValues, float * yValues, int * IntensityValues);
int FeasaCom_Sequence_ReadHSI(const int CommPort, const int Fiber, float * HueValues, int * SaturationValues, int * IntensityValues);
int FeasaCom_Sequence_ReadRGBI(const int CommPort, const int Fiber, unsigned char * RedValues, unsigned char * GreenValues, unsigned char * BlueValues, int * IntensityValues);
int FeasaCom_Sequence_ReadCCT(const int CommPort, const int Fiber, int * CCTValues, float * deltauvValues);
int FeasaCom_Sequence_ReadWavelength(const int CommPort, const int Fiber, int * WavelengthValues);
int FeasaCom_Sequence_GetPattern(const int CommPort, const int * IntensityValues, int * StatusCount, int * PatternTimes, int * PatternIntensities);
int FeasaCom_Sequence_GetSweepingPattern(const int CommPort, const int LEDCount, const int isOffToOn, int * LowTimes, int * HighTimes, int * IntensityValues);
int FeasaCom_Sequence_GetFrequency(const int CommPort, const int * IntensityValues, float * Frequency, float * DC, int * CycleCount);
int FeasaCom_Sequence_FindTestSettings(const int CommPort, const int TotalLEDCount, const int FiberToTest, const int SignalSpeed, const int BlinkingSpeed, const int MinCycleCount, const int TimeResolutionIsImportant, int * CaptureTime, int * WaitTime, int * SampleCount);
int FeasaCom_Sequence_SetPatternThresholdHigh(const int CommPort, const int Intensity);
int FeasaCom_Sequence_SetPatternThresholdLow(const int CommPort, const int Intensity);

// Daisy-chain functions
int FeasaCom_DaisyChain_Add(const int CommPort, const char * SerialNumber);
int FeasaCom_DaisyChain_Del(const int CommPort, const char * SerialNumber);
int FeasaCom_DaisyChain_Clear(const int CommPort);
int FeasaCom_DaisyChain_Send(int CommPort, const char * SerialNumber, const char * Command, char * ResponseText);
int FeasaCom_DaisyChain_Capture(const int CommPort, const int isPWM, const int CaptureRange, const int CapturePWMFrames);
int FeasaCom_DaisyChain_SpectrometerCapture(const int CommPort, const int isPWM, const int UsePresetExposure, const float ExposureTime);
int FeasaCom_DaisyChain_SpectrometerDark(const int CommPor, const int isPWMt, const int UsePresetExposure, const float ExposureTime);

// External Trigger functions
int FeasaCom_ExternalTrigger_Listen(const int CommPort);
int FeasaCom_ExternalTrigger_Abort(const int CommPort);
int FeasaCom_ExternalTrigger_isFinished(const int CommPort);

// Comm handling functions
int FeasaCom_EnumPorts(void);
void FeasaCom_EnumPorts_Filter(const int USB, const int RS232, const int Bluetooth);
int FeasaCom_IsConnected(const char * SerialNumber, const char * Baudrate);
int FeasaCom_AreConnected(int * PortNumbers, const char ** SerialNumbers, const int nSerials, const char * Baudrate);
int FeasaCom_AreConnectedS(int * PortNumbers, const char * SerialNumbers, const char * Baudrate);
int FeasaCom_Detect(int * CommPorts, const char * Baudrate);
int FeasaCom_DetectSN(char ** SerialNumbers, const char * Baudrate);
void FeasaCom_AddDetectionFilter(const char * Filter);
void FeasaCom_ClearDetectionFilters(void);
int FeasaCom_IsPortAvailable(const int CommPort);
int FeasaCom_ListPortsDetected(int * ListOfPortsDetected);
int FeasaCom_ListPortsDetectedTxt(char * ListOfPortsDetected, const char * Delimiter);
int FeasaCom_SetResponseTimeout(const unsigned int Timeout);
int FeasaCom_SetResponseTimeoutAuto(const int CommPort, const int status);
long FeasaCom_GetBaudrate(const int CommPort);
int FeasaCom_GetDeviceType(const int CommPort, char * DeviceType);
void FeasaCom_GetError_Description(char * ErrorDescription);
void FeasaCom_GetError_DescriptionByPort(const int CommPort, char * ErrorDescription);
void FeasaCom_GetError_DescriptionBySN(const char * SerialNumber, char * ErrorDescription);
int FeasaCom_GetPortBySN(const char * SerialNumber);
int FeasaCom_GetSNByPort(char * SerialNumber, const int CommPort);
int FeasaCom_GetPortByID(const char * DeviceID);
int FeasaCom_OpenProject(const char * Path);
int FeasaCom_CloseProject(void);
int FeasaCom_SendByID(const char * DeviceID, const char * Command, char * ResponseText);

// Binning
int FeasaCom_Binning_GetBinFromVECFile(const char * Path, const float x, const float y, char * ResultBinName);

// UserCal functions
int FeasaCom_UserCal_ResetxyOffsets(const int CommPort, const int Fiber, const int toFlash);
int FeasaCom_UserCal_SetxyOffsets(const int CommPort, const int Fiber, const float xOffset, const float yOffset, const int toFlash);
int FeasaCom_UserCal_GetxyOffsets(const int CommPort, const int Fiber, float * xOffset, float * yOffset);
int FeasaCom_UserCal_AdjustxyOffsets(const int CommPort, const int Fiber, const float xRef, const float yRef, const int toFlash);

int FeasaCom_UserCal_ResetWavelengthOffset(const int CommPort, const int Fiber, const int toFlash);
int FeasaCom_UserCal_GetWavelengthOffset(const int CommPort, const int Fiber, int * WavelengthOffset);
int FeasaCom_UserCal_SetWavelengthOffset(const int CommPort, const int Fiber, int const WavelengthOffset, const int toFlash);
int FeasaCom_UserCal_AdjustWavelengthOffset(const int CommPort, const int Fiber, int const WavelengthRef, const int toFlash);

int FeasaCom_UserCal_ResetIntensity(const int CommPort, const int Fiber, const int toFlash);
int FeasaCom_UserCal_GetIntensityGain(const int CommPort, const int Fiber, int * Gain);
int FeasaCom_UserCal_SetIntensityGain(const int CommPort, const int Fiber, int const Gain, const int toFlash);
int FeasaCom_UserCal_AdjustIntensity(const int CommPort, const int Fiber, int const IntensityRef, const int isPWM, const int CaptureRange, const int toFlash);

int FeasaCom_UserCal_ResetAbsInt(const int CommPort, const int Fiber, const int toFlash);
int FeasaCom_UserCal_GetAbsIntFactor(const int CommPort, const int Fiber, double * Factor);
int FeasaCom_UserCal_SetAbsIntFactor(const int CommPort, const int Fiber, double const Factor, const int toFlash);
int FeasaCom_UserCal_AdjustAbsInt(const int CommPort, const int Fiber, double const AbsIntRef, const int toFlash);

int FeasaCom_UserCal_ResetRGBAdj(const int CommPort, const int Fiber);
int FeasaCom_UserCal_TakeRGBCurrentValues(const int CommPort, const int Fiber, const char Color);
int FeasaCom_UserCal_AdjustRGB(const int CommPort, const int Fiber, const float xRefRed, const float yRefRed, const double AbsIntRefRed, const float xRefGreen, const float yRefGreen, const double AbsIntRefGreen, const float xRefBlue, const float yRefBlue, const double AbsIntRefBlue);

#endif //FEASACOM_H