{********************************************************
*  Copyright 2020 Feasa Enterprises Ltd
*  Feasa Communications Library
********************************************************}

unit Feasa;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

  // ---------- FEASACOM ----------
  procedure FeasaCom_GetDLLVersion(Version:PAnsiChar) stdcall; external 'feasacom64.dll';
  
  // Basic Comm functions
  function FeasaCom_Open(CommPort:Integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Close(CommPort:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Send(CommPort:Integer; Command:AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';

  function FeasaCom_OpenSN(SerialNumber:AnsiString; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CloseSN(SerialNumber:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SendSN(SerialNumber:AnsiString; Command:AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';

  // Comm helper functions
  function FeasaCom_SendToAll(ReturnValues:PInteger; Command:AnsiString; Responses:PPAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SendToAll_NR(ReturnValues: PInteger; Command: AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Open_Multi(ReturnValues:PInteger; CommPorts:PInteger; nPorts:Integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Close_Multi(ReturnValues:PInteger; CommPorts:PInteger; nPorts:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Send_Multi(ReturnValues:PInteger; CommPorts:PInteger; nPorts:Integer; Commands:PPAnsiChar; Responses:PPAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Send_Multi_NR(ReturnValues:PInteger; CommPorts:PInteger; nPorts:Integer; Commands:AnsiString; CommandSeparator:AnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_OpenSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CloseSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SendSN_Multi(ReturnValues:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer; Commands:PPAnsiChar; Responses:PPAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CloseAll():Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetResponseByPort(CommPort: integer; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';

  // Test functions
  function FeasaCom_Capture(CommPort: integer; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CaptureFromAll(ReturnValues: PInteger; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SpectrometerCapture(CommPort: integer; isPWM: integer; UseCustomExposure: integer; ExposureTime: Single):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SpectrometerDark(CommPort: integer; isPWM: integer; UseCustomExposure: integer; ExposureTime: Single):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CaptureFromAllSpectrometers(ReturnValues: PInteger; UseCustomExposure: integer; ExposureTime: Single):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_Setup(CommPort: integer; StartDelay: integer; CaptureTime: integer; TimeBetweenCaptures: integer; SampleCount: integer; toFlash: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_Capture(CommPort: integer; Fiber: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadIntensity(CommPort: integer; Fiber: integer; IntensityValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadxyI(CommPort: integer; Fiber: integer; xValues: PSingle; yValues: PSingle; IntensityValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadHSI(CommPort: integer; Fiber: integer; HueValues: PSingle; SaturationValues: PInteger; IntensityValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadRGBI(CommPort: integer; Fiber: integer; RedValues: PByte; GreenValues: PByte; BlueValues: PByte; IntensityValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadCCT(CommPort: integer; Fiber: integer; CCTValues: PInteger; deltauvValues: PSingle):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_ReadWavelength(CommPort: integer; Fiber: integer; WavelengthValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_GetPattern(CommPort: integer; IntensityValues: PInteger; StatusCount: PInteger; PatternTimes: PInteger; PatternIntensities: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_GetSweepingPattern(CommPort: integer; LEDCount: integer; isOffToOn: integer; LowTimes: PInteger; HighTimes: PInteger; IntensityValues: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_GetFrequency(CommPort: integer; IntensityValues: PInteger; Frequency: PSingle; DC: PSingle; CycleCount: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_FindTestSettings(CommPort: integer; TotalLEDCount: integer; FiberToTest: integer; SignalSpeed: integer; BlinkingSpeed: integer; MinCycleCount: integer; TimeResolutionIsImportant: integer; CaptureTime: PInteger; WaitTime: PInteger; SampleCount: PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_SetPatternThresholdHigh(CommPort: integer; Intensity: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Sequence_SetPatternThresholdLow(CommPort: integer; Intensity: integer):Integer stdcall; external 'feasacom64.dll';

  // Daisy-chain functions
  function FeasaCom_DaisyChain_Add(CommPort:Integer; SerialNumber:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_Del(CommPort:Integer; SerialNumber:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_Clear(CommPort:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_Send(CommPort: integer; SerialNumber: AnsiString; Command: AnsiString; ResponseText: PAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_Capture(CommPort:Integer; isPWM:Integer; CaptureRange:Integer; CapturePWMFrames:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_SpectrometerCapture(CommPort: integer; isPWM:Integer; UsePresetExposure: integer; ExposureTime: Single):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DaisyChain_SpectrometerDark(CommPort: integer; isPWM:Integer; UsePresetExposure: integer; ExposureTime: Single):Integer stdcall; external 'feasacom64.dll';

  // External trigger functions
  function FeasaCom_ExternalTrigger_Listen(CommPort: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_ExternalTrigger_Abort(CommPort: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_ExternalTrigger_isFinished(CommPort: integer):Integer stdcall; external 'feasacom64.dll';

  // Comm handling functions
  function FeasaCom_EnumPorts():Integer stdcall; external 'feasacom64.dll';
  procedure FeasaCom_EnumPorts_Filter(USB:Integer; RS232:Integer; Bluetooth:Integer) stdcall; external 'feasacom64.dll';
  function FeasaCom_IsConnected(SerialNumber:AnsiString; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_AreConnected(PortNumbers:PInteger; SerialNumbers:PPAnsiChar; nSerials:Integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_AreConnectedS(PortNumbers:PInteger; SerialNumbers:AnsiString; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Detect(CommPorts:PInteger; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DetectSN(SerialNumbers:PPAnsiChar; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  procedure FeasaCom_AddDetectionFilter(Filter:AnsiString) stdcall; external 'feasacom64.dll';
  procedure FeasaCom_ClearDetectionFilters() stdcall; external 'feasacom64.dll';
  function FeasaCom_IsPortAvailable(CommPort:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_ListPortsDetected(ListOfPortsDetected:PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_ListPortsDetectedTxt(ListOfPortsDetected:PAnsiChar; Delimiter:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SetResponseTimeout(Timeout:Word):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SetResponseTimeoutAuto(CommPort:integer; Status:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetBaudrate(CommPort:Integer):LongInt stdcall; external 'feasacom64.dll';
  function FeasaCom_GetDeviceType(CommPort:integer; DeviceType:PAnsiChar):Integer stdcall; external 'feasacom64.dll';
  procedure FeasaCom_GetError_Description(ResponseText:PAnsiChar) stdcall; external 'feasacom64.dll';
  procedure FeasaCom_GetError_DescriptionByPort(CommPort:Integer; ResponseText:PAnsiChar) stdcall; external 'feasacom64.dll';
  procedure FeasaCom_GetError_DescriptionBySN(SerialNumber:AnsiString; ResponseText:PAnsiChar) stdcall; external 'feasacom64.dll';
  function FeasaCom_GetPortBySN(SerialNumber:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetSNByPort(SerialNumber:PAnsiChar; CommPort:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetPortByID(DeviceID: AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetOpenedPorts(CommPorts:PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_GetOpenedPortsS(CommPortsTxt:PAnsiChar; Delimiter:AnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_OpenProject(Path:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CloseProject():Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SendByID(DeviceID:AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';


  // Binning
  function FeasaCom_Binning_GetBinFromVECFile(Path:AnsiString; x:Single; y:Single; ResultBinName:PAnsiChar):Integer stdcall; external 'feasacom64.dll';


  // UserCal functions
  function FeasaCom_UserCal_ResetIntensity(CommPort:Integer; Fiber:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_GetIntensityGain(CommPort:Integer; Fiber:Integer; Gain:PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_SetIntensityGain(CommPort:Integer; Fiber:Integer; Gain:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_AdjustIntensity(CommPort:Integer; Fiber:Integer; IntensityRef:Integer; isPWM:Integer; CaptureRange:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';

  function FeasaCom_UserCal_ResetAbsInt(CommPort:Integer; Fiber:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_GetAbsIntFactor(CommPort:Integer; Fiber:Integer; Factor:PDouble):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_SetAbsIntFactor(CommPort:Integer; Fiber:Integer; Factor:Double; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_AdjustAbsInt(CommPort:Integer; Fiber:Integer; AbsIntRef:Double; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';

  function FeasaCom_UserCal_ResetWavelengthOffset(CommPort:Integer; Fiber:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_GetWavelengthOffset(CommPort:Integer; Fiber:Integer; WavelengthOffset:PInteger):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_SetWavelengthOffset(CommPort:Integer; Fiber:Integer; WavelengthOffset:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_AdjustWavelengthOffset(CommPort:Integer; Fiber:Integer; WavelengthRef:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';

  function FeasaCom_UserCal_ResetxyOffsets(CommPort:Integer; Fiber:Integer; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_GetxyOffsets(CommPort:Integer; Fiber:Integer; xOffset:PSingle; yOffset:PSingle):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_SetxyOffsets(CommPort:Integer; Fiber:Integer; xOffset:Single; yOffset:Single; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_AdjustxyOffsets(CommPort:Integer; Fiber:Integer; xRef:Single; yRef:Single; toFlash:Integer):Integer stdcall; external 'feasacom64.dll';

  function FeasaCom_UserCal_ResetRGBAdj(CommPort: integer; Fiber: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_TakeRGBCurrentValues(CommPort: integer; Fiber: integer; Color:AnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_UserCal_AdjustRGB(CommPort: integer; Fiber: integer; xRefRed:Single; yRefRed:Single; AbsIntRefRed:Double; xRefGreen:Single; yRefGreen:Single; AbsIntRefGreen:Double; xRefBlue:Single; yRefBlue:Single; AbsIntRefBlue:Double):Integer stdcall; external 'feasacom64.dll';

  
  // ---------- FEASA TOOLS ----------
  function Feasa_Parse_Int16(StringToParse: AnsiString; Parameter: Byte):SmallInt stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_Int32(StringToParse: AnsiString; Parameter: Byte):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_IntFloat(StringToParse: AnsiString; Parameter: Byte):Single stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_RGBI(Response: AnsiString; Red: PByte; Green: PByte; Blue: PByte; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_RGBI_All(Response: AnsiString; RedValues: PByte; GreenValues: PByte; BlueValues: PByte; IntensityValues:PInteger):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_HSI(Response: AnsiString; Hue: PSingle; Saturation: PInteger; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_HSI_All(Response: AnsiString; HueValues: PSingle; SaturationValues: PInteger; IntensityValues:PInteger):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_xy(Response: AnsiString; x: PSingle; y: PSingle):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_xy_All(Response: AnsiString; xValues: PSingle; yValues: PSingle):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_uv(Response: AnsiString; u: PSingle; v: PSingle):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_uv_All(Response: AnsiString; uValues: PSingle; vValues: PSingle):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_CCT(Response: AnsiString; CCT: PInteger; delta: PSingle):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_CCT_All(Response: AnsiString; CCTValues: PInteger; deltaValues: PSingle):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_Wavelength(Response: AnsiString; Wavelength:PSingle):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_Wavelength_All(Response: AnsiString; WavelengthValues:PSingle):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_WI(Response: AnsiString; Wavelength:PSingle; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_WI_All(Response: AnsiString; WavelengthValues:PSingle; IntensityValues:PInteger):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_Intensity(Response: AnsiString; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_Intensity_All(Response: AnsiString; IntensityValues:PInteger):Integer stdcall; external 'feasa_tools64.dll';

  function Feasa_Parse_Spectrum(Response: AnsiString; WavelengthValues:PSingle; IntensityValues:PDouble):Integer stdcall; external 'feasa_tools64.dll';

  
  // ---------- LOCAL FUNCTIONS ----------
  procedure InitializeArrayOfStrings(mArray:PPAnsiChar; ItemCount:Integer; StringSize:Integer);
  function FormatDecimal(TxtNumber:string) : double;
  function FormatDecimalTxt(TxtNumber:string) : double;
  function CstringToString(cstring:AnsiString) : string;
  
implementation

procedure InitializeArrayOfStrings(mArray:PPAnsiChar; ItemCount:Integer; StringSize:Integer);
var
  i: Integer;
begin
     for i := 0 to (ItemCount-1) do
         GetMem(mArray[i], StringSize);
end;

function FormatDecimal(TxtNumber:string) : double;
var
aux:string;
auxnum:Double;
begin

     if TryStrToFloat('20.34', auxnum) then
     begin
          aux := StringReplace(TxtNumber,',','.',[rfReplaceAll, rfIgnoreCase]);
     end else
     begin
          aux := StringReplace(TxtNumber,'.',',',[rfReplaceAll, rfIgnoreCase]);
     end;

     Result := StrToFloat(aux);
end;

function FormatDecimalTxt(TxtNumber:string) : double;
var
aux:string;
auxnum:string;
begin

     if TryStrToFloat('20.34', auxnum) then
     begin
          aux := StringReplace(TxtNumber,',','.',[rfReplaceAll, rfIgnoreCase]);
     end else
     begin
          aux := StringReplace(TxtNumber,'.',',',[rfReplaceAll, rfIgnoreCase]);
     end;

     Result := aux;
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

