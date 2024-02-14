/********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Communications Library
********************************************************/

#include <stdio.h>
#include <windows.h>
#include <string.h>
#include "feasacom.h"

tFeasaCom_GetDLLVersion FeasaCom_GetDLLVersion;

// Basic Comm functions
tFeasaCom_OpenSN FeasaCom_OpenSN;
tFeasaCom_SendSN FeasaCom_SendSN;
tFeasaCom_CloseSN FeasaCom_CloseSN;
tFeasaCom_Open FeasaCom_Open;
tFeasaCom_Send FeasaCom_Send;
tFeasaCom_Close FeasaCom_Close;

// Comm helper functions
tFeasaCom_SendToAll FeasaCom_SendToAll;
tFeasaCom_SendToAll_NR FeasaCom_SendToAll_NR;
tFeasaCom_Open_Multi FeasaCom_Open_Multi;
tFeasaCom_Close_Multi FeasaCom_Close_Multi;
tFeasaCom_Send_Multi FeasaCom_Send_Multi;
tFeasaCom_Send_Multi_NR FeasaCom_Send_Multi_NR;
tFeasaCom_OpenSN_Multi FeasaCom_OpenSN_Multi;
tFeasaCom_CloseSN_Multi FeasaCom_CloseSN_Multi;
tFeasaCom_SendSN_Multi FeasaCom_SendSN_Multi;
tFeasaCom_CloseAll FeasaCom_CloseAll;
tFeasaCom_GetResponseByPort FeasaCom_GetResponseByPort;

// Test functions
tFeasaCom_Capture FeasaCom_Capture;
tFeasaCom_CaptureFromAll FeasaCom_CaptureFromAll;
tFeasaCom_SpectrometerCapture FeasaCom_SpectrometerCapture;
tFeasaCom_SpectrometerDark FeasaCom_SpectrometerDark;
tFeasaCom_CaptureFromAllSpectrometers FeasaCom_CaptureFromAllSpectrometers;
tFeasaCom_Sequence_Setup FeasaCom_Sequence_Setup;
tFeasaCom_Sequence_Capture FeasaCom_Sequence_Capture;
tFeasaCom_Sequence_ReadIntensity FeasaCom_Sequence_ReadIntensity;
tFeasaCom_Sequence_ReadxyI FeasaCom_Sequence_ReadxyI;
tFeasaCom_Sequence_ReadHSI FeasaCom_Sequence_ReadHSI;
tFeasaCom_Sequence_ReadRGBI FeasaCom_Sequence_ReadRGBI;
tFeasaCom_Sequence_ReadCCT FeasaCom_Sequence_ReadCCT;
tFeasaCom_Sequence_ReadWavelength FeasaCom_Sequence_ReadWavelength;
tFeasaCom_Sequence_GetPattern FeasaCom_Sequence_GetPattern;
tFeasaCom_Sequence_GetSweepingPattern FeasaCom_Sequence_GetSweepingPattern;
tFeasaCom_Sequence_GetFrequency FeasaCom_Sequence_GetFrequency;
tFeasaCom_Sequence_FindTestSettings FeasaCom_Sequence_FindTestSettings;
tFeasaCom_Sequence_SetPatternThresholdHigh FeasaCom_Sequence_SetPatternThresholdHigh;
tFeasaCom_Sequence_SetPatternThresholdLow FeasaCom_Sequence_SetPatternThresholdLow;

// Daisy-chain functions
tFeasaCom_DaisyChain_Add FeasaCom_DaisyChain_Add;
tFeasaCom_DaisyChain_Del FeasaCom_DaisyChain_Del;
tFeasaCom_DaisyChain_Clear FeasaCom_DaisyChain_Clear;
tFeasaCom_DaisyChain_Send FeasaCom_DaisyChain_Send;
tFeasaCom_DaisyChain_Capture FeasaCom_DaisyChain_Capture;
tFeasaCom_DaisyChain_SpectrometerCapture FeasaCom_DaisyChain_SpectrometerCapture;
tFeasaCom_DaisyChain_SpectrometerDark FeasaCom_DaisyChain_SpectrometerDark;

// External Trigger functions
tFeasaCom_ExternalTrigger_Listen FeasaCom_ExternalTrigger_Listen;
tFeasaCom_ExternalTrigger_Abort FeasaCom_ExternalTrigger_Abort;
tFeasaCom_ExternalTrigger_isFinished FeasaCom_ExternalTrigger_isFinished;

// Comm handling functions
tFeasaCom_EnumPorts FeasaCom_EnumPorts;
tFeasaCom_EnumPorts_Filter FeasaCom_EnumPorts_Filter;
tFeasaCom_IsConnected FeasaCom_IsConnected;
tFeasaCom_AreConnected FeasaCom_AreConnected;
tFeasaCom_AreConnectedS FeasaCom_AreConnectedS;
tFeasaCom_Detect FeasaCom_Detect;
tFeasaCom_DetectSN FeasaCom_DetectSN;
tFeasaCom_AddDetectionFilter FeasaCom_AddDetectionFilter;
tFeasaCom_ClearDetectionFilters FeasaCom_ClearDetectionFilters;
tFeasaCom_IsPortAvailable FeasaCom_IsPortAvailable;
tFeasaCom_ListPortsDetected FeasaCom_ListPortsDetected;
tFeasaCom_ListPortsDetectedTxt FeasaCom_ListPortsDetectedTxt;
tFeasaCom_SetResponseTimeout FeasaCom_SetResponseTimeout;
tFeasaCom_SetResponseTimeoutAuto FeasaCom_SetResponseTimeoutAuto;
tFeasaCom_GetBaudrate FeasaCom_GetBaudrate;
tFeasaCom_GetDeviceType FeasaCom_GetDeviceType;
tFeasaCom_GetError_Description FeasaCom_GetError_Description;
tFeasaCom_GetError_DescriptionByPort FeasaCom_GetError_DescriptionByPort;
tFeasaCom_GetError_DescriptionBySN FeasaCom_GetError_DescriptionBySN;
tFeasaCom_GetPortBySN FeasaCom_GetPortBySN;
tFeasaCom_GetSNByPort FeasaCom_GetSNByPort;
tFeasaCom_GetPortByID FeasaCom_GetPortByID;
tFeasaCom_GetOpenedPorts FeasaCom_GetOpenedPorts;
tFeasaCom_GetOpenedPortsS FeasaCom_GetOpenedPortsS;
tFeasaCom_OpenProject FeasaCom_OpenProject;
tFeasaCom_CloseProject FeasaCom_CloseProject;
tFeasaCom_SendByID FeasaCom_SendByID;

// Binning
tFeasaCom_Binning_GetBinFromVECFile FeasaCom_Binning_GetBinFromVECFile;

// UserCal functions
tFeasaCom_UserCal_ResetxyOffsets FeasaCom_UserCal_ResetxyOffsets;
tFeasaCom_UserCal_SetxyOffsets FeasaCom_UserCal_SetxyOffsets;
tFeasaCom_UserCal_GetxyOffsets FeasaCom_UserCal_GetxyOffsets;
tFeasaCom_UserCal_AdjustxyOffsets FeasaCom_UserCal_AdjustxyOffsets;

tFeasaCom_UserCal_ResetWavelengthOffset FeasaCom_UserCal_ResetWavelengthOffset;
tFeasaCom_UserCal_GetWavelengthOffset FeasaCom_UserCal_GetWavelengthOffset;
tFeasaCom_UserCal_SetWavelengthOffset FeasaCom_UserCal_SetWavelengthOffset;
tFeasaCom_UserCal_AdjustWavelengthOffset FeasaCom_UserCal_AdjustWavelengthOffset;

tFeasaCom_UserCal_ResetIntensity FeasaCom_UserCal_ResetIntensity;
tFeasaCom_UserCal_GetIntensityGain FeasaCom_UserCal_GetIntensityGain;
tFeasaCom_UserCal_SetIntensityGain FeasaCom_UserCal_SetIntensityGain;
tFeasaCom_UserCal_AdjustIntensity FeasaCom_UserCal_AdjustIntensity;

tFeasaCom_UserCal_ResetAbsInt FeasaCom_UserCal_ResetAbsInt;
tFeasaCom_UserCal_GetAbsIntFactor FeasaCom_UserCal_GetAbsIntFactor;
tFeasaCom_UserCal_SetAbsIntFactor FeasaCom_UserCal_SetAbsIntFactor;
tFeasaCom_UserCal_AdjustAbsInt FeasaCom_UserCal_AdjustAbsInt;

tFeasaCom_UserCal_ResetRGBAdj FeasaCom_UserCal_ResetRGBAdj;
tFeasaCom_UserCal_TakeRGBCurrentValues FeasaCom_UserCal_TakeRGBCurrentValues;
tFeasaCom_UserCal_AdjustRGB FeasaCom_UserCal_AdjustRGB;

HINSTANCE hFeasaComDLL;

int FeasaCom_Load(int is64bit, char * PathToDLL)
{	
	int l = strlen(PathToDLL);

	char * DLLPath = (char *)malloc(sizeof(char) * (l + 100));
	DLLPath[0] = '\0';

	if (l > 0) {
		strcpy(DLLPath, PathToDLL);
		if ((DLLPath[l - 1] != '/') && (DLLPath[l - 1] != '\\'))
			strcat(DLLPath, "\\");
	}

	if ((strstr(DLLPath, ".dll") == NULL) && (strstr(DLLPath, ".dll") == NULL)) {
		if (is64bit)
			strcat(DLLPath, "feasacom64.dll");
		else
			strcat(DLLPath, "feasacom.dll");
	}

	hFeasaComDLL = LoadLibrary(DLLPath);
	if (hFeasaComDLL == NULL) {
		free(DLLPath);
		return 0;
	}
	// Get Addresses of DLL functions
	FeasaCom_GetDLLVersion = (tFeasaCom_GetDLLVersion)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetDLLVersion");
	if (FeasaCom_GetDLLVersion == NULL) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}
	
	// Basic Comm functions
	FeasaCom_OpenSN = (tFeasaCom_OpenSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_OpenSN");
	FeasaCom_CloseSN = (tFeasaCom_CloseSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseSN");
	FeasaCom_SendSN = (tFeasaCom_SendSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendSN");
	FeasaCom_Open = (tFeasaCom_Open)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Open");
	FeasaCom_Close = (tFeasaCom_Close)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Close");
	FeasaCom_Send = (tFeasaCom_Send)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send");
	if ((FeasaCom_OpenSN == NULL) || (FeasaCom_CloseSN == NULL) || (FeasaCom_SendSN == NULL) || (FeasaCom_Open == NULL) || (FeasaCom_Close == NULL) || (FeasaCom_Send == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// Comm helper functions
	FeasaCom_SendToAll = (tFeasaCom_SendToAll)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendToAll");
	FeasaCom_SendToAll_NR = (tFeasaCom_SendToAll_NR)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendToAll_NR");
	FeasaCom_Open_Multi = (tFeasaCom_Open_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Open_Multi");
	FeasaCom_Close_Multi = (tFeasaCom_Close_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Close_Multi");
	FeasaCom_Send_Multi = (tFeasaCom_Send_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send_Multi");
	FeasaCom_Send_Multi_NR = (tFeasaCom_Send_Multi_NR)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Send_Multi_NR");
	FeasaCom_OpenSN_Multi = (tFeasaCom_OpenSN_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_OpenSN_Multi");
	FeasaCom_CloseSN_Multi = (tFeasaCom_CloseSN_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseSN_Multi");
	FeasaCom_SendSN_Multi = (tFeasaCom_SendSN_Multi)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendSN_Multi");
	FeasaCom_CloseAll = (tFeasaCom_CloseAll)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseAll");
	FeasaCom_GetResponseByPort = (tFeasaCom_GetResponseByPort)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetResponseByPort");
	if ((FeasaCom_SendToAll == NULL) || (FeasaCom_SendToAll_NR == NULL) || (FeasaCom_Open_Multi == NULL) || (FeasaCom_Close_Multi == NULL) || (FeasaCom_Send_Multi == NULL) || (FeasaCom_Send_Multi_NR == NULL) || (FeasaCom_OpenSN_Multi == NULL) || (FeasaCom_CloseSN_Multi == NULL) || (FeasaCom_SendSN_Multi == NULL) || (FeasaCom_CloseAll == NULL) || (FeasaCom_GetResponseByPort == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}
	
	// Test functions
	FeasaCom_Capture = (tFeasaCom_Capture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Capture");
	FeasaCom_CaptureFromAll = (tFeasaCom_CaptureFromAll)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CaptureFromAll");
	FeasaCom_SpectrometerCapture = (tFeasaCom_SpectrometerCapture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SpectrometerCapture");
	FeasaCom_SpectrometerDark = (tFeasaCom_SpectrometerDark)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SpectrometerDark");
	FeasaCom_CaptureFromAllSpectrometers = (tFeasaCom_CaptureFromAllSpectrometers)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CaptureFromAllSpectrometers");
	FeasaCom_Sequence_Setup = (tFeasaCom_Sequence_Setup)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_Setup");
	FeasaCom_Sequence_Capture = (tFeasaCom_Sequence_Capture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_Capture");
	FeasaCom_Sequence_ReadIntensity = (tFeasaCom_Sequence_ReadIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadIntensity");
	FeasaCom_Sequence_ReadxyI = (tFeasaCom_Sequence_ReadxyI)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadxyI");
	FeasaCom_Sequence_ReadHSI = (tFeasaCom_Sequence_ReadHSI)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadHSI");
	FeasaCom_Sequence_ReadRGBI = (tFeasaCom_Sequence_ReadRGBI)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadRGBI");
	FeasaCom_Sequence_ReadCCT = (tFeasaCom_Sequence_ReadCCT)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadCCT");
	FeasaCom_Sequence_ReadWavelength = (tFeasaCom_Sequence_ReadWavelength)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_ReadWavelength");
	FeasaCom_Sequence_GetPattern = (tFeasaCom_Sequence_GetPattern)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetPattern");
	FeasaCom_Sequence_GetSweepingPattern = (tFeasaCom_Sequence_GetSweepingPattern)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetSweepingPattern");
	FeasaCom_Sequence_GetFrequency = (tFeasaCom_Sequence_GetFrequency)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_GetFrequency");
	FeasaCom_Sequence_FindTestSettings = (tFeasaCom_Sequence_FindTestSettings)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_FindTestSettings");
	FeasaCom_Sequence_SetPatternThresholdHigh = (tFeasaCom_Sequence_SetPatternThresholdHigh)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_SetPatternThresholdHigh");
	FeasaCom_Sequence_SetPatternThresholdLow = (tFeasaCom_Sequence_SetPatternThresholdLow)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Sequence_SetPatternThresholdLow");
	if ((FeasaCom_Capture == NULL) || (FeasaCom_CaptureFromAll == NULL) || (FeasaCom_SpectrometerCapture == NULL) || (FeasaCom_SpectrometerDark == NULL) || (FeasaCom_CaptureFromAllSpectrometers == NULL) || (FeasaCom_Sequence_Setup == NULL) || (FeasaCom_Sequence_Capture == NULL) || (FeasaCom_Sequence_ReadIntensity == NULL) || (FeasaCom_Sequence_ReadxyI == NULL) || (FeasaCom_Sequence_ReadHSI == NULL) || (FeasaCom_Sequence_ReadRGBI == NULL) || (FeasaCom_Sequence_ReadCCT == NULL) || (FeasaCom_Sequence_ReadWavelength == NULL) || (FeasaCom_Sequence_GetPattern == NULL) || (FeasaCom_Sequence_GetSweepingPattern == NULL) || (FeasaCom_Sequence_GetFrequency == NULL) || (FeasaCom_Sequence_FindTestSettings == NULL) || (FeasaCom_Sequence_SetPatternThresholdHigh == NULL) || (FeasaCom_Sequence_SetPatternThresholdLow == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// Daisy-chain functions
	FeasaCom_DaisyChain_Add = (tFeasaCom_DaisyChain_Add)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_Add");
	FeasaCom_DaisyChain_Del = (tFeasaCom_DaisyChain_Del)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_Del");
	FeasaCom_DaisyChain_Clear = (tFeasaCom_DaisyChain_Clear)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_Clear");
	FeasaCom_DaisyChain_Send = (tFeasaCom_DaisyChain_Send)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_Send");
	FeasaCom_DaisyChain_Capture = (tFeasaCom_DaisyChain_Capture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_Capture");
	FeasaCom_DaisyChain_SpectrometerCapture = (tFeasaCom_DaisyChain_SpectrometerCapture)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_SpectrometerCapture");
	FeasaCom_DaisyChain_SpectrometerDark = (tFeasaCom_DaisyChain_SpectrometerDark)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DaisyChain_SpectrometerDark");
	if ((FeasaCom_DaisyChain_Add == NULL) || (FeasaCom_DaisyChain_Del == NULL) || (FeasaCom_DaisyChain_Clear == NULL) || (FeasaCom_DaisyChain_Send == NULL) || (FeasaCom_DaisyChain_Capture == NULL) || (FeasaCom_DaisyChain_SpectrometerCapture == NULL) || (FeasaCom_DaisyChain_SpectrometerDark == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// External Trigger functions
	FeasaCom_ExternalTrigger_Listen = (tFeasaCom_ExternalTrigger_Listen)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ExternalTrigger_Listen");
	FeasaCom_ExternalTrigger_Abort = (tFeasaCom_ExternalTrigger_Abort)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ExternalTrigger_Abort");
	FeasaCom_ExternalTrigger_isFinished = (tFeasaCom_ExternalTrigger_isFinished)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ExternalTrigger_isFinished");
	if ((FeasaCom_ExternalTrigger_Listen == NULL) || (FeasaCom_ExternalTrigger_Abort == NULL) || (FeasaCom_ExternalTrigger_isFinished == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// Comm handling functions
	FeasaCom_EnumPorts = (tFeasaCom_EnumPorts)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_EnumPorts");
	FeasaCom_EnumPorts_Filter = (tFeasaCom_EnumPorts_Filter)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_EnumPorts_Filter");
	FeasaCom_IsConnected = (tFeasaCom_IsConnected)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_IsConnected");
	FeasaCom_AreConnected = (tFeasaCom_AreConnected)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_AreConnected");
	FeasaCom_AreConnectedS = (tFeasaCom_AreConnectedS)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_AreConnectedS");
	FeasaCom_Detect = (tFeasaCom_Detect)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Detect");
	FeasaCom_DetectSN = (tFeasaCom_DetectSN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_DetectSN");
	FeasaCom_AddDetectionFilter = (tFeasaCom_AddDetectionFilter)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_AddDetectionFilter");
	FeasaCom_ClearDetectionFilters = (tFeasaCom_ClearDetectionFilters)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ClearDetectionFilters");
	FeasaCom_IsPortAvailable = (tFeasaCom_IsPortAvailable)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_IsPortAvailable");
	FeasaCom_ListPortsDetected = (tFeasaCom_ListPortsDetected)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ListPortsDetected");
	FeasaCom_ListPortsDetectedTxt = (tFeasaCom_ListPortsDetectedTxt)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_ListPortsDetectedTxt");
	FeasaCom_SetResponseTimeout = (tFeasaCom_SetResponseTimeout)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SetResponseTimeout");
	FeasaCom_SetResponseTimeoutAuto = (tFeasaCom_SetResponseTimeoutAuto)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SetResponseTimeoutAuto");
	FeasaCom_GetBaudrate = (tFeasaCom_GetBaudrate)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetBaudrate");
	FeasaCom_GetDeviceType = (tFeasaCom_GetDeviceType)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetDeviceType");
	FeasaCom_GetError_Description = (tFeasaCom_GetError_Description)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetError_Description");
	FeasaCom_GetError_DescriptionByPort = (tFeasaCom_GetError_DescriptionByPort)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetError_DescriptionByPort");
	FeasaCom_GetError_DescriptionBySN = (tFeasaCom_GetError_DescriptionBySN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetError_DescriptionBySN");
	FeasaCom_GetPortBySN = (tFeasaCom_GetPortBySN)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetPortBySN");
	FeasaCom_GetSNByPort = (tFeasaCom_GetSNByPort)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetSNByPort");
	FeasaCom_GetPortByID = (tFeasaCom_GetPortByID)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetPortByID");
	FeasaCom_GetOpenedPorts = (tFeasaCom_GetOpenedPorts)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetOpenedPorts");
	FeasaCom_GetOpenedPortsS = (tFeasaCom_GetOpenedPortsS)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_GetOpenedPortsS");
	FeasaCom_OpenProject = (tFeasaCom_OpenProject)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_OpenProject");
	FeasaCom_CloseProject = (tFeasaCom_CloseProject)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_CloseProject");
	FeasaCom_SendByID = (tFeasaCom_SendByID)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_SendByID");
	if ((FeasaCom_EnumPorts == NULL) || (FeasaCom_EnumPorts_Filter == NULL) || (FeasaCom_IsConnected == NULL) || (FeasaCom_AreConnected == NULL) || (FeasaCom_Detect == NULL) || (FeasaCom_DetectSN == NULL) || (FeasaCom_AddDetectionFilter == NULL) || (FeasaCom_ClearDetectionFilters == NULL) || (FeasaCom_IsPortAvailable == NULL) || (FeasaCom_ListPortsDetected == NULL) || (FeasaCom_ListPortsDetectedTxt == NULL) || (FeasaCom_SetResponseTimeout == NULL) || (FeasaCom_SetResponseTimeoutAuto == NULL) || (FeasaCom_GetBaudrate == NULL) || (FeasaCom_GetDeviceType == NULL) || (FeasaCom_GetError_Description == NULL) || (FeasaCom_GetError_DescriptionByPort == NULL) || (FeasaCom_GetError_DescriptionBySN == NULL) || (FeasaCom_GetPortBySN == NULL) || (FeasaCom_GetSNByPort == NULL) || (FeasaCom_GetPortByID == NULL) || (FeasaCom_GetOpenedPorts == NULL) || (FeasaCom_GetOpenedPortsS == NULL) || (FeasaCom_OpenProject == NULL) || (FeasaCom_CloseProject == NULL) || (FeasaCom_SendByID == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// Binning
	FeasaCom_Binning_GetBinFromVECFile = (tFeasaCom_Binning_GetBinFromVECFile)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_Binning_GetBinFromVECFile");
	if (FeasaCom_Binning_GetBinFromVECFile == NULL) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	// UserCal functions
	FeasaCom_UserCal_ResetxyOffsets = (tFeasaCom_UserCal_ResetxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetxyOffsets");
	FeasaCom_UserCal_SetxyOffsets = (tFeasaCom_UserCal_SetxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_SetxyOffsets");
	FeasaCom_UserCal_GetxyOffsets = (tFeasaCom_UserCal_GetxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetxyOffsets");
	FeasaCom_UserCal_AdjustxyOffsets = (tFeasaCom_UserCal_AdjustxyOffsets)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustxyOffsets");

	FeasaCom_UserCal_ResetWavelengthOffset = (tFeasaCom_UserCal_ResetWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetWavelengthOffset");
	FeasaCom_UserCal_GetWavelengthOffset = (tFeasaCom_UserCal_GetWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetWavelengthOffset");
	FeasaCom_UserCal_SetWavelengthOffset = (tFeasaCom_UserCal_SetWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_SetWavelengthOffset");
	FeasaCom_UserCal_AdjustWavelengthOffset = (tFeasaCom_UserCal_AdjustWavelengthOffset)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustWavelengthOffset");

	FeasaCom_UserCal_ResetIntensity = (tFeasaCom_UserCal_ResetIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetIntensity");
	FeasaCom_UserCal_GetIntensityGain = (tFeasaCom_UserCal_GetIntensityGain)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetIntensityGain");
	FeasaCom_UserCal_SetIntensityGain = (tFeasaCom_UserCal_SetIntensityGain)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_SetIntensityGain");
	FeasaCom_UserCal_AdjustIntensity = (tFeasaCom_UserCal_AdjustIntensity)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustIntensity");

	FeasaCom_UserCal_ResetAbsInt = (tFeasaCom_UserCal_ResetAbsInt)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetAbsInt");
	FeasaCom_UserCal_GetAbsIntFactor = (tFeasaCom_UserCal_GetAbsIntFactor)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_GetAbsIntFactor");
	FeasaCom_UserCal_SetAbsIntFactor = (tFeasaCom_UserCal_SetAbsIntFactor)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_SetAbsIntFactor");
	FeasaCom_UserCal_AdjustAbsInt = (tFeasaCom_UserCal_AdjustAbsInt)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustAbsInt");
	
	FeasaCom_UserCal_ResetRGBAdj = (tFeasaCom_UserCal_ResetRGBAdj)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_ResetRGBAdj");
	FeasaCom_UserCal_TakeRGBCurrentValues = (tFeasaCom_UserCal_TakeRGBCurrentValues)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_TakeRGBCurrentValues");
	FeasaCom_UserCal_AdjustRGB = (tFeasaCom_UserCal_AdjustRGB)GetProcAddress((HMODULE)hFeasaComDLL, "FeasaCom_UserCal_AdjustRGB");
	if ((FeasaCom_UserCal_ResetxyOffsets == NULL) || (FeasaCom_UserCal_SetxyOffsets == NULL) || (FeasaCom_UserCal_GetxyOffsets == NULL) || (FeasaCom_UserCal_AdjustxyOffsets == NULL) || (FeasaCom_UserCal_ResetWavelengthOffset == NULL) || (FeasaCom_UserCal_GetWavelengthOffset == NULL) || (FeasaCom_UserCal_SetWavelengthOffset == NULL) || (FeasaCom_UserCal_AdjustWavelengthOffset == NULL) || (FeasaCom_UserCal_ResetIntensity == NULL) || (FeasaCom_UserCal_GetIntensityGain == NULL) || (FeasaCom_UserCal_SetIntensityGain == NULL) || (FeasaCom_UserCal_AdjustIntensity == NULL) || (FeasaCom_UserCal_ResetAbsInt == NULL) || (FeasaCom_UserCal_GetAbsIntFactor == NULL) || (FeasaCom_UserCal_SetAbsIntFactor == NULL) || (FeasaCom_UserCal_AdjustAbsInt == NULL) || (FeasaCom_UserCal_ResetRGBAdj == NULL) || (FeasaCom_UserCal_TakeRGBCurrentValues == NULL) || (FeasaCom_UserCal_AdjustRGB == NULL)) {
		FreeLibrary((HMODULE)hFeasaComDLL);
		free(DLLPath);
		return 0;
	}

	free(DLLPath);

	return 1;
}

void FeasaCom_UnLoad()
{
	FeasaCom_CloseAll();
	FreeLibrary((HMODULE)hFeasaComDLL);
}


void FormatDecimal(char * buffer)
{
	int i, l;
	float f = 0;
	char decChr, c;

	l = strlen(buffer);

	if (l>0) {
		sscanf("0.253", "%f", &f);

		if (f == (float)(0.253)) {
			decChr = '.'; c = ',';
		}
		else {
			decChr = ','; c = '.';
		}

		for (i = 0; i<l; i++) {
			if (buffer[i] == c) buffer[i] = decChr;
		}
	}
}
