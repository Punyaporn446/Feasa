#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

typedef int (__stdcall *tFeasaCom_EnumPorts)(void);
typedef int (__stdcall *tFeasaCom_IsConnected)(char * SerialNumber, char * Baudrate);
typedef int (__stdcall *tFeasaCom_OpenSN)(char * SerialNumber, char * Baudrate);
typedef int (__stdcall *tFeasaCom_CloseSN)(char * SerialNumber);
typedef int (__stdcall *tFeasaCom_SendSN)(char * SerialNumber, char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_Open)(int CommPort, char * Baudrate);
typedef int (__stdcall *tFeasaCom_Close)(int CommPort);
typedef int (__stdcall *tFeasaCom_Send)(int CommPort, char * Command, char * ResponseText);
typedef int (__stdcall *tFeasaCom_CloseAll)(void);
typedef int (__stdcall *tFeasaCom_SetResponseTimeout)(unsigned int Timeout);
typedef int (__stdcall *tFeasaCom_IsPortAvailable)(int CommPort);
typedef int (__stdcall *tFeasaCom_ListPortsDetected)(int * ListOfPortsDetected);
typedef int (__stdcall *tFeasaCom_ListPortsDetectedTxt)(char * ListOfPortsDetected, const char * Delimiter);
typedef long (__stdcall *tFeasaCom_GetBaudrate)(int CommPort);
typedef void (__stdcall *tFeasaCom_GetErrorDescription)(char * ErrorDescription);
typedef int (__stdcall *tFeasaCom_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);
typedef int (__stdcall *tFeasaCom_DaisyChain_Add)(int CommPort, char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Del)(int CommPort, char * SerialNumber);
typedef int (__stdcall *tFeasaCom_DaisyChain_Clear)(int CommPort);
typedef int (__stdcall *tFeasaCom_DaisyChain_Capture)(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

#endif //FEASACOM_H