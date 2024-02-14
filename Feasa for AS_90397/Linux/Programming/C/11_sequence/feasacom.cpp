#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <string.h>
#include "feasacom.h"

/********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Dynamic Library
********************************************************/

tFeasaCom_GetLibraryVersion FeasaCom_GetLibraryVersion;

// Declare pointers to call functions
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
tFeasaCom_GetOpenedPorts FeasaCom_GetOpenedPorts;
tFeasaCom_GetOpenedPortsS FeasaCom_GetOpenedPortsS;
tFeasaCom_OpenProject FeasaCom_OpenProject;
tFeasaCom_CloseProject FeasaCom_CloseProject;
tFeasaCom_SendByID FeasaCom_SendByID;

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
tFeasaCom_ExternalTrigger_Enable FeasaCom_ExternalTrigger_Enable;
tFeasaCom_ExternalTrigger_Disable FeasaCom_ExternalTrigger_Disable;

// Comm handling functions
tFeasaCom_EnumPorts FeasaCom_EnumPorts;
tFeasaCom_IsConnected FeasaCom_IsConnected;
tFeasaCom_AreConnected FeasaCom_AreConnected;
tFeasaCom_AreConnectedS FeasaCom_AreConnectedS;
tFeasaCom_Detect FeasaCom_Detect;
tFeasaCom_DetectS FeasaCom_DetectS;
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

void * hFeasaCom;

int FeasaCom_Load(const char * PathToLibrary)
{
	int l = strlen(PathToLibrary);

	char * SOPath = (char *)malloc(sizeof(char) * (l + 100));
	SOPath[0] = '\0';

	if (l > 0) {
		strcpy(SOPath, PathToLibrary);
		if ((SOPath[l - 1] != '/') && (SOPath[l - 1] != '\\'))
			strcat(SOPath, "\\");
	}

	if ((strstr(SOPath, ".so") == NULL) && (strstr(SOPath, ".so") == NULL)) {
	#ifdef __LP64__
	strcat(SOPath, "libfeasacom.x86_64.so");
	#else
	strcat(SOPath, "libfeasacom.so");
	#endif // __LP64__
	}

	hFeasaCom = dlopen(SOPath, RTLD_GLOBAL | RTLD_LAZY);
	if (hFeasaCom == NULL) {
		free(SOPath);
		return 0;
	}
	
	// Get Addresses of SO Library functions

	FeasaCom_GetLibraryVersion = (tFeasaCom_GetLibraryVersion)dlsym(hFeasaCom, "FeasaCom_GetLibraryVersion");
	if (FeasaCom_GetLibraryVersion == NULL) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// Declare pointers to call functions
	FeasaCom_OpenSN = (tFeasaCom_OpenSN)dlsym(hFeasaCom, "FeasaCom_OpenSN");
	FeasaCom_CloseSN = (tFeasaCom_CloseSN)dlsym(hFeasaCom, "FeasaCom_CloseSN");
	FeasaCom_SendSN = (tFeasaCom_SendSN)dlsym(hFeasaCom, "FeasaCom_SendSN");
	FeasaCom_Open = (tFeasaCom_Open)dlsym(hFeasaCom, "FeasaCom_Open");
	FeasaCom_Close = (tFeasaCom_Close)dlsym(hFeasaCom, "FeasaCom_Close");
	FeasaCom_Send = (tFeasaCom_Send)dlsym(hFeasaCom, "FeasaCom_Send");
	if ((FeasaCom_OpenSN == NULL) || (FeasaCom_CloseSN == NULL) || (FeasaCom_SendSN == NULL) || (FeasaCom_Open == NULL) || (FeasaCom_Close == NULL) || (FeasaCom_Send == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// Comm helper functions
	FeasaCom_SendToAll = (tFeasaCom_SendToAll)dlsym(hFeasaCom, "FeasaCom_SendToAll");
	FeasaCom_SendToAll_NR = (tFeasaCom_SendToAll_NR)dlsym(hFeasaCom, "FeasaCom_SendToAll_NR");
	FeasaCom_Open_Multi = (tFeasaCom_Open_Multi)dlsym(hFeasaCom, "FeasaCom_Open_Multi");
	FeasaCom_Close_Multi = (tFeasaCom_Close_Multi)dlsym(hFeasaCom, "FeasaCom_Close_Multi");
	FeasaCom_Send_Multi = (tFeasaCom_Send_Multi)dlsym(hFeasaCom, "FeasaCom_Send_Multi");
	FeasaCom_Send_Multi_NR = (tFeasaCom_Send_Multi_NR)dlsym(hFeasaCom, "FeasaCom_Send_Multi_NR");
	FeasaCom_OpenSN_Multi = (tFeasaCom_OpenSN_Multi)dlsym(hFeasaCom, "FeasaCom_OpenSN_Multi");
	FeasaCom_CloseSN_Multi = (tFeasaCom_CloseSN_Multi)dlsym(hFeasaCom, "FeasaCom_CloseSN_Multi");
	FeasaCom_SendSN_Multi = (tFeasaCom_SendSN_Multi)dlsym(hFeasaCom, "FeasaCom_SendSN_Multi");
	FeasaCom_CloseAll = (tFeasaCom_CloseAll)dlsym(hFeasaCom, "FeasaCom_CloseAll");
	FeasaCom_GetResponseByPort = (tFeasaCom_GetResponseByPort)dlsym(hFeasaCom, "FeasaCom_GetResponseByPort");
	FeasaCom_GetOpenedPorts = (tFeasaCom_GetOpenedPorts)dlsym(hFeasaCom, "FeasaCom_GetOpenedPorts");
	FeasaCom_GetOpenedPortsS = (tFeasaCom_GetOpenedPortsS)dlsym(hFeasaCom, "FeasaCom_GetOpenedPortsS");
	FeasaCom_OpenProject = (tFeasaCom_OpenProject)dlsym(hFeasaCom, "FeasaCom_OpenProject");
	FeasaCom_CloseProject = (tFeasaCom_CloseProject)dlsym(hFeasaCom, "FeasaCom_CloseProject");
	FeasaCom_SendByID = (tFeasaCom_SendByID)dlsym(hFeasaCom, "FeasaCom_SendByID");
	if ((FeasaCom_SendToAll == NULL) || (FeasaCom_SendToAll_NR == NULL) || (FeasaCom_Open_Multi == NULL) || (FeasaCom_Close_Multi == NULL) || (FeasaCom_Send_Multi == NULL) || (FeasaCom_Send_Multi_NR == NULL) || (FeasaCom_OpenSN_Multi == NULL) || (FeasaCom_CloseSN_Multi == NULL) || (FeasaCom_SendSN_Multi == NULL) || (FeasaCom_CloseAll == NULL) || (FeasaCom_GetResponseByPort == NULL) || (FeasaCom_OpenProject == NULL) || (FeasaCom_CloseProject == NULL) || (FeasaCom_SendByID == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}
	
	// Test functions
	FeasaCom_Capture = (tFeasaCom_Capture)dlsym(hFeasaCom, "FeasaCom_Capture");
	FeasaCom_CaptureFromAll = (tFeasaCom_CaptureFromAll)dlsym(hFeasaCom, "FeasaCom_CaptureFromAll");
	FeasaCom_SpectrometerCapture = (tFeasaCom_SpectrometerCapture)dlsym(hFeasaCom, "FeasaCom_SpectrometerCapture");
	FeasaCom_SpectrometerDark = (tFeasaCom_SpectrometerDark)dlsym(hFeasaCom, "FeasaCom_SpectrometerDark");
	FeasaCom_CaptureFromAllSpectrometers = (tFeasaCom_CaptureFromAllSpectrometers)dlsym(hFeasaCom, "FeasaCom_CaptureFromAllSpectrometers");
	FeasaCom_Sequence_Setup = (tFeasaCom_Sequence_Setup)dlsym(hFeasaCom, "FeasaCom_Sequence_Setup");
	FeasaCom_Sequence_Capture = (tFeasaCom_Sequence_Capture)dlsym(hFeasaCom, "FeasaCom_Sequence_Capture");
	FeasaCom_Sequence_ReadIntensity = (tFeasaCom_Sequence_ReadIntensity)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadIntensity");
	FeasaCom_Sequence_ReadxyI = (tFeasaCom_Sequence_ReadxyI)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadxyI");
	FeasaCom_Sequence_ReadHSI = (tFeasaCom_Sequence_ReadHSI)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadHSI");
	FeasaCom_Sequence_ReadRGBI = (tFeasaCom_Sequence_ReadRGBI)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadRGBI");
	FeasaCom_Sequence_ReadCCT = (tFeasaCom_Sequence_ReadCCT)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadCCT");
	FeasaCom_Sequence_ReadWavelength = (tFeasaCom_Sequence_ReadWavelength)dlsym(hFeasaCom, "FeasaCom_Sequence_ReadWavelength");
	FeasaCom_Sequence_GetPattern = (tFeasaCom_Sequence_GetPattern)dlsym(hFeasaCom, "FeasaCom_Sequence_GetPattern");
	FeasaCom_Sequence_GetSweepingPattern = (tFeasaCom_Sequence_GetSweepingPattern)dlsym(hFeasaCom, "FeasaCom_Sequence_GetSweepingPattern");
	FeasaCom_Sequence_GetFrequency = (tFeasaCom_Sequence_GetFrequency)dlsym(hFeasaCom, "FeasaCom_Sequence_GetFrequency");
	FeasaCom_Sequence_FindTestSettings = (tFeasaCom_Sequence_FindTestSettings)dlsym(hFeasaCom, "FeasaCom_Sequence_FindTestSettings");
	FeasaCom_Sequence_SetPatternThresholdHigh = (tFeasaCom_Sequence_SetPatternThresholdHigh)dlsym(hFeasaCom, "FeasaCom_Sequence_SetPatternThresholdHigh");
	FeasaCom_Sequence_SetPatternThresholdLow = (tFeasaCom_Sequence_SetPatternThresholdLow)dlsym(hFeasaCom, "FeasaCom_Sequence_SetPatternThresholdLow");
	if ((FeasaCom_Capture == NULL) || (FeasaCom_CaptureFromAll == NULL) || (FeasaCom_SpectrometerCapture == NULL) || (FeasaCom_SpectrometerDark == NULL) || (FeasaCom_CaptureFromAllSpectrometers == NULL) || (FeasaCom_Sequence_Setup == NULL) || (FeasaCom_Sequence_Capture == NULL) || (FeasaCom_Sequence_ReadIntensity == NULL) || (FeasaCom_Sequence_ReadxyI == NULL) || (FeasaCom_Sequence_ReadHSI == NULL) || (FeasaCom_Sequence_ReadRGBI == NULL) || (FeasaCom_Sequence_ReadCCT == NULL) || (FeasaCom_Sequence_ReadWavelength == NULL) || (FeasaCom_Sequence_GetPattern == NULL) || (FeasaCom_Sequence_GetSweepingPattern == NULL) || (FeasaCom_Sequence_GetFrequency == NULL) || (FeasaCom_Sequence_FindTestSettings == NULL) || (FeasaCom_Sequence_SetPatternThresholdHigh == NULL) || (FeasaCom_Sequence_SetPatternThresholdLow == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// Daisy-chain functions
	FeasaCom_DaisyChain_Add = (tFeasaCom_DaisyChain_Add)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Add");
	FeasaCom_DaisyChain_Del = (tFeasaCom_DaisyChain_Del)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Del");
	FeasaCom_DaisyChain_Clear = (tFeasaCom_DaisyChain_Clear)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Clear");
	FeasaCom_DaisyChain_Send = (tFeasaCom_DaisyChain_Send)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Send");
	FeasaCom_DaisyChain_Capture = (tFeasaCom_DaisyChain_Capture)dlsym(hFeasaCom, "FeasaCom_DaisyChain_Capture");
	FeasaCom_DaisyChain_SpectrometerCapture = (tFeasaCom_DaisyChain_SpectrometerCapture)dlsym(hFeasaCom, "FeasaCom_DaisyChain_SpectrometerCapture");
	FeasaCom_DaisyChain_SpectrometerDark = (tFeasaCom_DaisyChain_SpectrometerDark)dlsym(hFeasaCom, "FeasaCom_DaisyChain_SpectrometerDark");
	if ((FeasaCom_DaisyChain_Add == NULL) || (FeasaCom_DaisyChain_Del == NULL) || (FeasaCom_DaisyChain_Clear == NULL) || (FeasaCom_DaisyChain_Send == NULL) || (FeasaCom_DaisyChain_Capture == NULL) || (FeasaCom_DaisyChain_SpectrometerCapture == NULL) || (FeasaCom_DaisyChain_SpectrometerDark == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// External Trigger functions
	FeasaCom_ExternalTrigger_Listen = (tFeasaCom_ExternalTrigger_Listen)dlsym(hFeasaCom, "FeasaCom_ExternalTrigger_Listen");
	FeasaCom_ExternalTrigger_Abort = (tFeasaCom_ExternalTrigger_Abort)dlsym(hFeasaCom, "FeasaCom_ExternalTrigger_Abort");
	FeasaCom_ExternalTrigger_isFinished = (tFeasaCom_ExternalTrigger_isFinished)dlsym(hFeasaCom, "FeasaCom_ExternalTrigger_isFinished");
	FeasaCom_ExternalTrigger_Enable = (tFeasaCom_ExternalTrigger_Enable)dlsym(hFeasaCom, "FeasaCom_ExternalTrigger_Enable");
	FeasaCom_ExternalTrigger_Disable = (tFeasaCom_ExternalTrigger_Disable)dlsym(hFeasaCom, "FeasaCom_ExternalTrigger_Disable");
	if ((FeasaCom_ExternalTrigger_Listen == NULL) || (FeasaCom_ExternalTrigger_Abort == NULL) || (FeasaCom_ExternalTrigger_isFinished == NULL) || (FeasaCom_ExternalTrigger_Enable == NULL) || (FeasaCom_ExternalTrigger_Disable == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// Comm handling functions
	FeasaCom_EnumPorts = (tFeasaCom_EnumPorts)dlsym(hFeasaCom, "FeasaCom_EnumPorts");
	FeasaCom_IsConnected = (tFeasaCom_IsConnected)dlsym(hFeasaCom, "FeasaCom_IsConnected");
	FeasaCom_AreConnected = (tFeasaCom_AreConnected)dlsym(hFeasaCom, "FeasaCom_AreConnected");
	FeasaCom_AreConnectedS = (tFeasaCom_AreConnectedS)dlsym(hFeasaCom, "FeasaCom_AreConnectedS");
	FeasaCom_Detect = (tFeasaCom_Detect)dlsym(hFeasaCom, "FeasaCom_Detect");
	FeasaCom_DetectS = (tFeasaCom_DetectS)dlsym(hFeasaCom, "FeasaCom_DetectS");
	FeasaCom_DetectSN = (tFeasaCom_DetectSN)dlsym(hFeasaCom, "FeasaCom_DetectSN");
	FeasaCom_AddDetectionFilter = (tFeasaCom_AddDetectionFilter)dlsym(hFeasaCom, "FeasaCom_AddDetectionFilter");
	FeasaCom_ClearDetectionFilters = (tFeasaCom_ClearDetectionFilters)dlsym(hFeasaCom, "FeasaCom_ClearDetectionFilters");
	FeasaCom_IsPortAvailable = (tFeasaCom_IsPortAvailable)dlsym(hFeasaCom, "FeasaCom_IsPortAvailable");
	FeasaCom_ListPortsDetected = (tFeasaCom_ListPortsDetected)dlsym(hFeasaCom, "FeasaCom_ListPortsDetected");
	FeasaCom_ListPortsDetectedTxt = (tFeasaCom_ListPortsDetectedTxt)dlsym(hFeasaCom, "FeasaCom_ListPortsDetectedTxt");
	FeasaCom_SetResponseTimeout = (tFeasaCom_SetResponseTimeout)dlsym(hFeasaCom, "FeasaCom_SetResponseTimeout");
	FeasaCom_SetResponseTimeoutAuto = (tFeasaCom_SetResponseTimeoutAuto)dlsym(hFeasaCom, "FeasaCom_SetResponseTimeoutAuto");
	FeasaCom_GetBaudrate = (tFeasaCom_GetBaudrate)dlsym(hFeasaCom, "FeasaCom_GetBaudrate");
	FeasaCom_GetDeviceType = (tFeasaCom_GetDeviceType)dlsym(hFeasaCom, "FeasaCom_GetDeviceType");
	FeasaCom_GetError_Description = (tFeasaCom_GetError_Description)dlsym(hFeasaCom, "FeasaCom_GetError_Description");
	FeasaCom_GetError_DescriptionByPort = (tFeasaCom_GetError_DescriptionByPort)dlsym(hFeasaCom, "FeasaCom_GetError_DescriptionByPort");
	FeasaCom_GetError_DescriptionBySN = (tFeasaCom_GetError_DescriptionBySN)dlsym(hFeasaCom, "FeasaCom_GetError_DescriptionBySN");
	FeasaCom_GetPortBySN = (tFeasaCom_GetPortBySN)dlsym(hFeasaCom, "FeasaCom_GetPortBySN");
	FeasaCom_GetSNByPort = (tFeasaCom_GetSNByPort)dlsym(hFeasaCom, "FeasaCom_GetSNByPort");
	FeasaCom_GetPortByID = (tFeasaCom_GetPortByID)dlsym(hFeasaCom, "FeasaCom_GetPortByID");
	if ((FeasaCom_EnumPorts == NULL) || (FeasaCom_IsConnected == NULL) || (FeasaCom_AreConnected == NULL) || (FeasaCom_Detect == NULL) || (FeasaCom_DetectS == NULL) || (FeasaCom_DetectSN == NULL)  || (FeasaCom_AddDetectionFilter == NULL)  || (FeasaCom_ClearDetectionFilters == NULL) || (FeasaCom_IsPortAvailable == NULL) || (FeasaCom_ListPortsDetected == NULL) || (FeasaCom_ListPortsDetectedTxt == NULL) || (FeasaCom_SetResponseTimeout == NULL) || (FeasaCom_SetResponseTimeoutAuto == NULL) || (FeasaCom_GetBaudrate == NULL)|| (FeasaCom_GetDeviceType == NULL) || (FeasaCom_GetError_Description == NULL)|| (FeasaCom_GetError_DescriptionByPort == NULL)|| (FeasaCom_GetError_DescriptionBySN == NULL) || (FeasaCom_GetPortBySN == NULL) || (FeasaCom_GetSNByPort == NULL) || (FeasaCom_GetPortByID == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// Binning
	FeasaCom_Binning_GetBinFromVECFile = (tFeasaCom_Binning_GetBinFromVECFile)dlsym(hFeasaCom, "FeasaCom_Binning_GetBinFromVECFile");
	if (FeasaCom_Binning_GetBinFromVECFile == NULL) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	// UserCal functions
	FeasaCom_UserCal_ResetxyOffsets = (tFeasaCom_UserCal_ResetxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetxyOffsets");
	FeasaCom_UserCal_SetxyOffsets = (tFeasaCom_UserCal_SetxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_SetxyOffsets");
	FeasaCom_UserCal_GetxyOffsets = (tFeasaCom_UserCal_GetxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_GetxyOffsets");
	FeasaCom_UserCal_AdjustxyOffsets = (tFeasaCom_UserCal_AdjustxyOffsets)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustxyOffsets");

	FeasaCom_UserCal_ResetWavelengthOffset = (tFeasaCom_UserCal_ResetWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetWavelengthOffset");
	FeasaCom_UserCal_GetWavelengthOffset = (tFeasaCom_UserCal_GetWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_GetWavelengthOffset");
	FeasaCom_UserCal_SetWavelengthOffset = (tFeasaCom_UserCal_SetWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_SetWavelengthOffset");
	FeasaCom_UserCal_AdjustWavelengthOffset = (tFeasaCom_UserCal_AdjustWavelengthOffset)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustWavelengthOffset");

	FeasaCom_UserCal_ResetIntensity = (tFeasaCom_UserCal_ResetIntensity)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetIntensity");
	FeasaCom_UserCal_GetIntensityGain = (tFeasaCom_UserCal_GetIntensityGain)dlsym(hFeasaCom, "FeasaCom_UserCal_GetIntensityGain");
	FeasaCom_UserCal_SetIntensityGain = (tFeasaCom_UserCal_SetIntensityGain)dlsym(hFeasaCom, "FeasaCom_UserCal_SetIntensityGain");
	FeasaCom_UserCal_AdjustIntensity = (tFeasaCom_UserCal_AdjustIntensity)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustIntensity");

	FeasaCom_UserCal_ResetAbsInt = (tFeasaCom_UserCal_ResetAbsInt)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetAbsInt");
	FeasaCom_UserCal_GetAbsIntFactor = (tFeasaCom_UserCal_GetAbsIntFactor)dlsym(hFeasaCom, "FeasaCom_UserCal_GetAbsIntFactor");
	FeasaCom_UserCal_SetAbsIntFactor = (tFeasaCom_UserCal_SetAbsIntFactor)dlsym(hFeasaCom, "FeasaCom_UserCal_SetAbsIntFactor");
	FeasaCom_UserCal_AdjustAbsInt = (tFeasaCom_UserCal_AdjustAbsInt)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustAbsInt");

	FeasaCom_UserCal_ResetRGBAdj = (tFeasaCom_UserCal_ResetRGBAdj)dlsym(hFeasaCom, "FeasaCom_UserCal_ResetRGBAdj");
	FeasaCom_UserCal_TakeRGBCurrentValues = (tFeasaCom_UserCal_TakeRGBCurrentValues)dlsym(hFeasaCom, "FeasaCom_UserCal_TakeRGBCurrentValues");
	FeasaCom_UserCal_AdjustRGB = (tFeasaCom_UserCal_AdjustRGB)dlsym(hFeasaCom, "FeasaCom_UserCal_AdjustRGB");
	if ((FeasaCom_UserCal_ResetxyOffsets == NULL) || (FeasaCom_UserCal_SetxyOffsets == NULL) || (FeasaCom_UserCal_GetxyOffsets == NULL) || (FeasaCom_UserCal_AdjustxyOffsets == NULL) || (FeasaCom_UserCal_ResetWavelengthOffset == NULL) || (FeasaCom_UserCal_GetWavelengthOffset == NULL) || (FeasaCom_UserCal_SetWavelengthOffset == NULL) || (FeasaCom_UserCal_AdjustWavelengthOffset == NULL) || (FeasaCom_UserCal_ResetIntensity == NULL) || (FeasaCom_UserCal_GetIntensityGain == NULL) || (FeasaCom_UserCal_SetIntensityGain == NULL) || (FeasaCom_UserCal_AdjustIntensity == NULL) || (FeasaCom_UserCal_ResetAbsInt == NULL) || (FeasaCom_UserCal_GetAbsIntFactor == NULL) || (FeasaCom_UserCal_SetAbsIntFactor == NULL) || (FeasaCom_UserCal_AdjustAbsInt == NULL) || (FeasaCom_UserCal_ResetRGBAdj == NULL) || (FeasaCom_UserCal_TakeRGBCurrentValues == NULL) || (FeasaCom_UserCal_AdjustRGB == NULL)) {
		dlclose(hFeasaCom);
		free(SOPath);
		return 0;
	}

	free(SOPath);

	return 1;
}

void FeasaCom_UnLoad()
{
	FeasaCom_CloseAll();
	dlclose(hFeasaCom);
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

/*void AllocateArrayOfStrings(char *** mArrayOfStrings, int SizeOfArray, int SizeOfString)
{
	int i;
	*mArrayOfStrings = (char **)malloc(size_t(sizeof(char *) * SizeOfArray));
	for (i = 0; i < SizeOfArray; i++)
		*mArrayOfStrings[i] = (char *)malloc(sizeof(char) * SizeOfString + 1);
}*/

void FreeArrayOfStrings(char ** mArrayOfStrings, const int Size)
{
	int i;
	for (i = 0; i < Size; i++)
		free(mArrayOfStrings[i]);
	free(mArrayOfStrings);
}

