unit Feasa;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

  // ---------- LOCAL FUNCTIONS ----------
  procedure InitializeArrayOfStrings(mArray:PPAnsiChar; ItemCount:Integer; StringSize:Integer);

  // ---------- FEASACOM ----------
  procedure FeasaCom_GetLibraryVersion(Version: PAnsiChar) cdecl; external 'libfeasacom.x86_64.so';

  // Basic Comm functions
  function FeasaCom_Open(DevPath: AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Send(DevPath: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Close(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_OpenSN(SerialNumber: AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SendSN(SerialNumber: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CloseSN(SerialNumber: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';

  // Comm helper functions
  function FeasaCom_SendToAll(ReturnValues: PInteger; Command: AnsiString; Responses:PPAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SendToAll_NR(ReturnValues: PInteger; Command: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Open_Multi(ReturnValues:PInteger; DevPaths:PPAnsiChar; nPorts:Integer; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Close_Multi(ReturnValues:PInteger; DevPaths:PPAnsiChar; nPorts:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Send_Multi(ReturnValues:PInteger; DevPaths:PPAnsiChar; nPorts:Integer; Commands:PPAnsiChar; Responses:PPAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Send_Multi_NR(ReturnValues:PInteger; DevPaths:PAnsiChar; nPorts:Integer; Commands:AnsiString; CommandSeparator:AnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_OpenSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CloseSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SendSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer; Commands:PPAnsiChar; Responses:PPAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CloseAll():Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetResponseByPort(DevPath: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetOpenedPorts(DevPaths:PPAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetOpenedPortsS(DevPaths:PAnsiChar; Delimiter:AnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_OpenProject(Path:AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CloseProject():Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SendByID(DeviceID:AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';

  // Test functions
  function FeasaCom_Capture(DevPath: AnsiString; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CaptureFromAll(ReturnValues: PInteger; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SpectrometerCapture(DevPath: AnsiString; isPWM: integer; UseCustomExposure: integer; ExposureTime: Single):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SpectrometerDark(DevPath: AnsiString; isPWM: integer; UseCustomExposure: integer; ExposureTime: Single):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_CaptureFromAllSpectrometers(ReturnValues: PInteger; isPWM: integer; UseCustomExposure: integer; ExposureTime: Single):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_Setup(DevPath: AnsiString; StartDelay: integer; CaptureTime: integer; TimeBetweenCaptures: integer; SampleCount: integer; toFlash: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_Capture(DevPath: AnsiString; Fiber: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadIntensity(DevPath: AnsiString; Fiber: integer; IntensityValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadxyI(DevPath: AnsiString; Fiber: integer; xValues: PSingle; yValues: PSingle; IntensityValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadHSI(DevPath: AnsiString; Fiber: integer; HueValues: PSingle; SaturationValues: PInteger; IntensityValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadRGBI(DevPath: AnsiString; Fiber: integer; RedValues: PByte; GreenValues: PByte; BlueValues: PByte; IntensityValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadCCT(DevPath: AnsiString; Fiber: integer; CCTValues: PInteger; deltauvValues: PSingle):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_ReadWavelength(DevPath: AnsiString; Fiber: integer; WavelengthValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_GetPattern(DevPath: AnsiString; IntensityValues: PInteger; StatusCount: PInteger; PatternTimes: PInteger; PatternIntensities: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_GetSweepingPattern(DevPath: AnsiString; LEDCount: integer; isOffToOn: integer; LowTimes: PInteger; HighTimes: PInteger; IntensityValues: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_GetFrequency(DevPath: AnsiString; IntensityValues: PInteger; Frequency: PSingle; DC: PSingle; CycleCount: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_FindTestSettings(DevPath: AnsiString; TotalLEDCount: integer; FiberToTest: integer; SignalSpeed: integer; BlinkingSpeed: integer; MinCycleCount: integer; TimeResolutionIsImportant: integer; CaptureTime: PInteger; WaitTime: PInteger; SampleCount: PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_SetPatternThresholdHigh(DevPath: AnsiString; Intensity: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Sequence_SetPatternThresholdLow(DevPath: AnsiString; Intensity: integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  // Daisy-chain functions
  function FeasaCom_DaisyChain_Add(DevPath: AnsiString; SerialNumber: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_Del(DevPath: AnsiString; SerialNumber: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_Clear(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_Send(DevPath: AnsiString; SerialNumber: AnsiString; Command: AnsiString; ResponseText: PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_Capture(DevPath: AnsiString; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_SpectrometerCapture(DevPath: AnsiString; UsePresetExposure: integer; UseCustomExposure: integer; ExposureTime: Single):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DaisyChain_SpectrometerDark(DevPath: AnsiString; isPWM:integer; UsePresetExposure: integer; ExposureTime: Single):Integer cdecl; external 'libfeasacom.x86_64.so';


  // External Trigger functions
  function FeasaCom_ExternalTrigger_Listen(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ExternalTrigger_Abort(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ExternalTrigger_isFinished(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ExternalTrigger_Enable(DevPath: AnsiString; CaptureRange: integer; isPWM: integer; OutputType: AnsiString; PreDelay: integer; PostDelay: integer; toFlash: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ExternalTrigger_Disable(DevPath: AnsiString; toFlash: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  

  // Comm handling functions
  function FeasaCom_EnumPorts():Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_IsConnected(DevPath: PAnsiChar; SerialNumber:AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_AreConnected(DevPaths: PPAnsiChar; SerialNumbers:PPAnsiChar; nSerials:Integer; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_AreConnectedS(DevPaths: PAnsiChar; SerialNumbers:AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Detect(DevPaths: PPAnsiChar; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DetectS(DevPaths: PAnsiChar; Delimiter: AnsiChar; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_DetectSN(SerialNumbers:PPAnsiChar; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  procedure FeasaCom_AddDetectionFilter(Filter:PAnsiChar) cdecl; external 'libfeasacom.x86_64.so';
  procedure FeasaCom_ClearDetectionFilters() cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_IsPortAvailable(DevPath:AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ListPortsDetected(ListOfPortsDetected:PPAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_ListPortsDetectedTxt(ListOfPortsDetected:PAnsiChar; Delimiter:AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SetResponseTimeout(Timeout:LongWord):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SetResponseTimeoutAuto(DevPath:AnsiString; Status:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetBaudrate(DevPath:AnsiString):LongInt cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetDeviceType(DevPath:AnsiString; DeviceType:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  procedure FeasaCom_GetError_Description(ResponseText:PAnsiChar) cdecl; external 'libfeasacom.x86_64.so';
  procedure FeasaCom_GetError_DescriptionByPort(DevPath:AnsiString; ResponseText:PAnsiChar) cdecl; external 'libfeasacom.x86_64.so';
  procedure FeasaCom_GetError_DescriptionBySN(SerialNumber:AnsiString; ResponseText:PAnsiChar) cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetPortBySN(DevPath: PAnsiChar; SerialNumber: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetSNByPort(SerialNumber:PAnsiChar; DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_GetPortByID(DeviceID: AnsiString; DevPath: PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';


  // Binning
  function FeasaCom_Binning_GetBinFromVECFile(Path: AnsiString; x:Single; y:Single; ResultBinName:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';


  // UserCal functions
  function FeasaCom_UserCal_ResetIntensity(DevPath: AnsiString; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetIntensityGain(DevPath: AnsiString; Fiber: integer; Gain:PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetIntensityGain(DevPath: AnsiString; Fiber: integer; Gain:integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustIntensity(DevPath: AnsiString; Fiber: integer; IntensityRef:integer; isPWM:integer; CaptureRange:integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetAbsInt(DevPath: AnsiString; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetAbsIntFactor(DevPath: AnsiString; Fiber: integer; Factor:PDouble):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetAbsIntFactor(DevPath: AnsiString; Fiber: integer; Factor:Double; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustAbsInt(DevPath: AnsiString; Fiber: integer; AbsIntRef:double; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetWavelengthOffset(DevPath: AnsiString; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetWavelengthOffset(DevPath: AnsiString; Fiber: integer; WavelengthOffset:PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetWavelengthOffset(DevPath: AnsiString; Fiber: integer; WavelengthOffset:Integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustWavelengthOffset(DevPath: AnsiString; Fiber: integer; WavelengthRef:Integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetxyOffsets(DevPath: AnsiString; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetxyOffsets(DevPath: AnsiString; Fiber: integer; xOffset:PSingle; yOffset:PSingle):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetxyOffsets(DevPath: AnsiString; Fiber: integer; xOffset:Single; yOffset:Single; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustxyOffsets(DevPath: AnsiString; Fiber: integer; xRef:Single; yRef:Single; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetRGBAdj(DevPath: AnsiString; Fiber: integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_TakeRGBCurrentValues(DevPath: AnsiString; Fiber: integer; Color:AnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustRGB(DevPath: AnsiString; Fiber: integer; xRefRed:Single; yRefRed:Single; AbsIntRefRed:Double; xRefGreen:Single; yRefGreen:Single; AbsIntRefGreen:Double; xRefBlue:Single; yRefBlue:Single; AbsIntRefBlue:Double):Integer cdecl; external 'libfeasacom.x86_64.so';

implementation

procedure InitializeArrayOfStrings(mArray:PPAnsiChar; ItemCount:Integer; StringSize:Integer);
var
  i: Integer;
begin
     for i := 0 to (ItemCount-1) do
         GetMem(mArray[i], StringSize);
end;

function CstringToString(cstring:AnsiString) : string;
var
  i: Integer;
  n: Integer;
  aux: string;
begin
     n := Length(cstring);
     aux := '';
     for i := 1 to n do
     begin
         if cstring[i]=Chr(0) then
         begin
               Result := aux;
               Exit;
         end;
         aux := aux + cstring[i];
     end;
end;

end.
