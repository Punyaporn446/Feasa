#ifndef FEASACOM_H
#define FEASACOM_H

/********************************************************
*  Copyright 2019 Feasa Enterprises Ltd
*  Led Analyser Dynamic Library
********************************************************/

extern int __stdcall FeasaCom_EnumPorts(void);
extern int __stdcall FeasaCom_IsConnected(char * SerialNumber, char * Baudrate);
extern int __stdcall FeasaCom_OpenSN(char * SerialNumber, char * Baudrate);
extern int __stdcall FeasaCom_CloseSN(char * SerialNumber);
extern int __stdcall FeasaCom_SendSN(char * SerialNumber, char * Command, char * ResponseText);
extern int __stdcall FeasaCom_Open(int CommPort, char * Baudrate);
extern int __stdcall FeasaCom_Close(int CommPort);
extern int __stdcall FeasaCom_Send(int CommPort, char * Command, char * ResponseText);
extern int __stdcall FeasaCom_CloseAll(void);
extern int __stdcall FeasaCom_SetResponseTimeout(unsigned int Timeout);

extern int __stdcall FeasaCom_IsPortAvailable(int CommPort);
extern int __stdcall FeasaCom_ListPortsDetected(int * ListOfPortsDetected);
extern int __stdcall FeasaCom_ListPortsDetectedTxt(char * ListOfPortsDetected, const char * Delimiter);

extern long __stdcall FeasaCom_GetBaudrate(int CommPort);

extern void __stdcall FeasaCom_GetError_Description(char * ErrorDescription);

extern int __stdcall FeasaCom_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

extern int __stdcall FeasaCom_DaisyChain_Add(int CommPort, char * SerialNumber);
extern int __stdcall FeasaCom_DaisyChain_Del(int CommPort, char * SerialNumber);
extern int __stdcall FeasaCom_DaisyChain_Clear(int CommPort);
extern int __stdcall FeasaCom_DaisyChain_Capture(int CommPort, int isPWM, int CaptureRange, int CapturePWMFrames);

#endif //FEASACOM_H
