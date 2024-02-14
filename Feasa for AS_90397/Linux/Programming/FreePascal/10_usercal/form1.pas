{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: UserCal
*
*  DESCRIPTION: This example demonstrates how to use the
*  UserCal library embedded in the Feasa SO Library in order
*  to ease the integration of the calibration process in any
*  user custom software.
*
*  This example uses a dynamic library to access to the
*  functions related to the LED Analyser. This library is read
*  in runtime, so you have to be sure that the file
*  libfeasacom_x86_64.so has been copied to the /usr/lib/
*  directory or equivalent, moreover,  some compillers/IDE
*  allow to reference the SO library from the same location
*  of the binary/script or alternative locations using absolute
*  or relative paths.
*
*  Note: there are 32 and 64-bit versions of the SO Library, so
*  one or the other has to be used depending on the compiler/IDE
*  platform or binary target platform.
*
***************************************************************}

unit form1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, SynEdit, SynHighlighterAny;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAdjustRGB: TButton;
    chkPWM: TCheckBox;
    btnBalanceInt: TButton;
    btnAdjustAbsInt: TButton;
    btnAdjustWavelength: TButton;
    btnAdjustxy: TButton;
    btnReadParams: TButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label9: TLabel;
    txtRefAbsInt: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lstCapture: TComboBox;
    txtLog: TMemo;
    numFibers: TSpinEdit;
    optRAM: TRadioButton;
    optFlash: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lstPort: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    txtRefAbsIntB: TEdit;
    txtRefAbsIntG: TEdit;
    txtRefAbsIntR: TEdit;
    txtRefWavelength: TEdit;
    txtRefx: TEdit;
    txtRefxB: TEdit;
    txtRefxG: TEdit;
    txtRefxR: TEdit;
    txtRefy: TEdit;
    txtRefyB: TEdit;
    txtRefyG: TEdit;
    txtRefyR: TEdit;
    procedure btnAdjustAbsIntClick(Sender: TObject);
    procedure btnAdjustRGBClick(Sender: TObject);
    procedure btnAdjustWavelengthClick(Sender: TObject);
    procedure btnAdjustxyClick(Sender: TObject);
    procedure btnBalanceIntClick(Sender: TObject);
    procedure btnReadParamsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function FormatDecimal(TxtNumber:string):double;
  private
    { private declarations }
  public
    { public declarations }
  end; 


  function FeasaCom_Open(DevPath: string; Baudrate:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Close(DevPath: string):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Send(DevPath: string; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_IsPortAvailable(DevPath: string):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Capture(DevPath: string; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetIntensity(DevPath: string; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetIntensityGain(DevPath: string; Fiber: integer; Gain:PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetIntensityGain(DevPath: string; Fiber: integer; Gain:integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustIntensity(DevPath: string; Fiber: integer; IntensityRef:integer; isPWM:integer; CaptureRange:integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetAbsInt(DevPath: string; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetAbsIntFactor(DevPath: string; Fiber: integer; Factor:PDouble):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetAbsIntFactor(DevPath: string; Fiber: integer; Factor:Double; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustAbsInt(DevPath: string; Fiber: integer; AbsIntRef:double; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetWavelengthOffset(DevPath: string; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetWavelengthOffset(DevPath: string; Fiber: integer; WavelengthOffset:PInteger):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetWavelengthOffset(DevPath: string; Fiber: integer; WavelengthOffset:Integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustWavelengthOffset(DevPath: string; Fiber: integer; WavelengthRef:Integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetxyOffsets(DevPath: string; Fiber: integer; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_GetxyOffsets(DevPath: string; Fiber: integer; xOffset:PSingle; yOffset:PSingle):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_SetxyOffsets(DevPath: string; Fiber: integer; xOffset:Single; yOffset:Single; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustxyOffsets(DevPath: string; Fiber: integer; xRef:Single; yRef:Single; toFlash:integer):Integer cdecl; external 'libfeasacom.x86_64.so';

  function FeasaCom_UserCal_ResetRGBAdj(DevPath: AnsiString; Fiber: integer):Integer stdcall; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_TakeRGBCurrentValues(DevPath: AnsiString; Fiber: integer; Color:AnsiChar):Integer stdcall; external 'libfeasacom.x86_64.so';
  function FeasaCom_UserCal_AdjustRGB(DevPath: AnsiString; Fiber: integer; xRefRed:Single; yRefRed:Single; AbsIntRefRed:Double; xRefGreen:Single; yRefGreen:Single; AbsIntRefGreen:Double; xRefBlue:Single; yRefBlue:Single; AbsIntRefBlue:Double):Integer stdcall; external 'libfeasacom.x86_64.so';

  function FeasaCom_GetError_Description(ErrorDescription: PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
//variables
var
i:integer;
begin
     //Add the port numbers in the list from 0 to 16
     for i:=0 to 50 do
     begin
          if FeasaCom_IsPortAvailable('/dev/ttyS' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyS' + IntToStr(i));
          if FeasaCom_IsPortAvailable('/dev/ttyUSB' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyUSB' + IntToStr(i));
     end;
     lstPort.ItemIndex:=0;

     //Add Capture types to the list
     lstCapture.Items.Add('AUTO');
     lstCapture.Items.Add('LOW');
     lstCapture.Items.Add('MEDIUM');
     lstCapture.Items.Add('HIGH');
     lstCapture.Items.Add('SUPER');
     lstCapture.Items.Add('ULTRA');
     lstCapture.ItemIndex:=0;

end;

procedure TfrmMain.btnBalanceIntClick(Sender: TObject);
var
i:integer;
DevPath:string;
NFIBERS:integer;
toFlash:integer;
ComResponse:PChar;
isPWM: integer;
CaptureRange:integer;
resp:integer;
AvgInt:integer;
const
  PWM_FRAMES = 5;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 255);

     //Read settings
     NFIBERS := numFibers.Value;
     if optFlash.Checked then
         toFlash := 1
     else
         toFlash := 0;
     if chkPWM.Checked then
         isPWM := 1
     else
         isPWM := 0;
     CaptureRange := lstCapture.ItemIndex;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //-------------------------------------------------
          // RELATIVE INTENSITY ADJUSTMENT
          //-------------------------------------------------

          //Reset intensities
          for i:=1 to NFIBERS do
               FeasaCom_UserCal_ResetIntensity(DevPath, i, toFlash);

          //Capture
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);

          //Read back results
          AvgInt := 0;
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_Send(DevPath, Format('GETINTENSITY%.2d', [i]), ComResponse);
               if resp=-1 then
               begin
                    MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end
               else if resp=0 then
               begin
                    MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
               AvgInt += StrToInt(ComResponse);
          end;
          AvgInt := AvgInt div NFIBERS;
          txtLog.Text := txtLog.Text + 'AvgInt=' + IntToStr(AvgInt) + #13#10;

          //Calibration
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_AdjustIntensity(DevPath, i, AvgInt, isPWM, CaptureRange, toFlash);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
          end;

          //Check results
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
          resp := FeasaCom_Send(DevPath,'GETINTENSITYALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;

     Freemem(ComResponse);

end;

procedure TfrmMain.btnAdjustAbsIntClick(Sender: TObject);
var
i:integer;
DevPath:string;
NFIBERS:integer;
toFlash:integer;
ComResponse:PChar;
isPWM: integer;
CaptureRange:integer;
resp:integer;
AbsIntRef:Double;
const
  PWM_FRAMES = 5;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 255);

     //Read settings
     NFIBERS := numFibers.Value;
     if optFlash.Checked then
         toFlash := 1
     else
         toFlash := 0;
     if chkPWM.Checked then
         isPWM := 1
     else
         isPWM := 0;
     CaptureRange := lstCapture.ItemIndex;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //-------------------------------------------------
          // RELATIVE INTENSITY ADJUSTMENT
          //-------------------------------------------------

          //Reset factors
          for i:=1 to NFIBERS do
               FeasaCom_UserCal_ResetAbsInt(DevPath, i, toFlash);

          //Capture
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);

          //Calibration
          AbsIntRef := FormatDecimal(txtRefAbsInt.Text);
          txtLog.Text := txtLog.Text + 'AbsIntRef=' + FloatToStr(AbsIntRef) + #13#10;
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_AdjustAbsInt(DevPath, i, AbsIntRef, toFlash);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
          end;

          //Check results
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
          resp := FeasaCom_Send(DevPath,'GETABSINTALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;


     Freemem(ComResponse);
end;

procedure TfrmMain.btnAdjustRGBClick(Sender: TObject);
var
i:integer;
c:integer;
DevPath:string;
NFIBERS:integer;
toFlash:integer;
ComResponse:PChar;
isPWM: integer;
CaptureRange:integer;
resp:integer;
xRefR:Single;
yRefR:Single;
AbsIntRefR:Double;
xRefG:Single;
yRefG:Single;
AbsIntRefG:Double;
xRefB:Single;
yRefB:Single;
AbsIntRefB:Double;
const
  PWM_FRAMES = 5;
  LEDCOLOR: Array[1..3] of AnsiString = ('RED', 'GREEN', 'BLUE');
begin
     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 1024);

     //Read settings
     NFIBERS := numFibers.Value;
     if optFlash.Checked then
         toFlash := 1
     else
         toFlash := 0;
     if chkPWM.Checked then
         isPWM := 1
     else
         isPWM := 0;
     CaptureRange := lstCapture.ItemIndex;

     //Open port
     if FeasaCom_Open(DevPath, 57600)=1 then
     begin
          //-------------------------------------------------
          // RGB ADJUSTMENT
          //-------------------------------------------------

          //Reset factors
          for i:=1 to NFIBERS do
               FeasaCom_UserCal_ResetWavelengthOffset(DevPath, i, toFlash);

          //Capture
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);

          //Calibration
          xRefR := FormatDecimal(txtRefxR.Text);
          yRefR := FormatDecimal(txtRefyR.Text);
          AbsIntRefR := FormatDecimal(txtRefAbsIntR.Text);
          xRefG := FormatDecimal(txtRefxG.Text);
          yRefG := FormatDecimal(txtRefyG.Text);
          AbsIntRefG := FormatDecimal(txtRefAbsIntG.Text);
          xRefB := FormatDecimal(txtRefxB.Text);
          yRefB := FormatDecimal(txtRefyB.Text);
          AbsIntRefB := FormatDecimal(txtRefAbsIntB.Text);
          txtLog.Text := txtLog.Text + 'RED: xRef=' + FloatToStr(xRefR) + ', yRef=' + FloatToStr(yRefR) + ', AbsIntRef=' + FloatToStr(AbsIntRefR) + #13#10;
          txtLog.Text := txtLog.Text + 'GREEN: xRef=' + FloatToStr(xRefG) + ', yRef=' + FloatToStr(yRefG) + ', AbsIntRef=' + FloatToStr(AbsIntRefG) + #13#10;
          txtLog.Text := txtLog.Text + 'BLUE: xRef=' + FloatToStr(xRefB) + ', yRef=' + FloatToStr(yRefB) + ', AbsIntRef=' + FloatToStr(AbsIntRefB) + #13#10;

          for c:=1 to 3 do
          begin
               MessageDlg('Please, switch on ' + LEDCOLOR[c] + ' LED and click OK to continue', mtInformation, [mbOK], 0);

              //Mesaure reference values
              resp := FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
              if not resp=1 then
              begin
                  MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
                  FeasaCom_Close(DevPath);
                  Freemem(ComResponse);
                  Abort;
              end;

              for i:=1 to NFIBERS do
              begin
                   resp := FeasaCom_UserCal_TakeRGBCurrentValues(DevPath, i, LEDCOLOR[c][1]);
                   if not resp=1 then
                   begin
                        FeasaCom_GetError_Description(ComResponse);
                        MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                        FeasaCom_Close(DevPath);
                        Freemem(ComResponse);
                        Abort;
                   end;
              end;
          end;

          //Calibration
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_AdjustRGB(DevPath, i, xRefR, yRefR, AbsIntRefR, xRefG, yRefG, AbsIntRefG, xRefB, yRefB, AbsIntRefB);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
          end;

          //check results
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
          if resp<>1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          resp := FeasaCom_Send(DevPath,'GETxyALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse + #13#10;
          resp := FeasaCom_Send(DevPath,'GETABSINTALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;


     Freemem(ComResponse);
end;

procedure TfrmMain.btnAdjustWavelengthClick(Sender: TObject);
var
i:integer;
DevPath:string;
NFIBERS:integer;
toFlash:integer;
ComResponse:PChar;
isPWM: integer;
CaptureRange:integer;
resp:integer;
Wref:integer;
const
  PWM_FRAMES = 5;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 255);

     //Read settings
     NFIBERS := numFibers.Value;
     if optFlash.Checked then
         toFlash := 1
     else
         toFlash := 0;
     if chkPWM.Checked then
         isPWM := 1
     else
         isPWM := 0;
     CaptureRange := lstCapture.ItemIndex;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //-------------------------------------------------
          // WAVELENGTH OFFSETS ADJUSTMENT
          //-------------------------------------------------

          //Reset factors
          for i:=1 to NFIBERS do
               FeasaCom_UserCal_ResetWavelengthOffset(DevPath, i, toFlash);

          //Capture
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);

          //Calibration
          Wref := StrToInt(txtRefWavelength.Text);
          txtLog.Text := txtLog.Text + 'Wref=' + IntToStr(Wref) + #13#10;
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_AdjustWavelengthOffset(DevPath, i, Wref, toFlash);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
          end;

          //Check results
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
          resp := FeasaCom_Send(DevPath,'GETwavelengthALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;


     Freemem(ComResponse);
end;

procedure TfrmMain.btnAdjustxyClick(Sender: TObject);
var
i:integer;
DevPath:string;
NFIBERS:integer;
toFlash:integer;
ComResponse:PChar;
isPWM: integer;
CaptureRange:integer;
resp:integer;
xRef:Single;
yRef:Single;
const
  PWM_FRAMES = 5;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 255);

     //Read settings
     NFIBERS := numFibers.Value;
     if optFlash.Checked then
         toFlash := 1
     else
         toFlash := 0;
     if chkPWM.Checked then
         isPWM := 1
     else
         isPWM := 0;
     CaptureRange := lstCapture.ItemIndex;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //-------------------------------------------------
          // xy OFFSETS ADJUSTMENT
          //-------------------------------------------------

          //Reset factors
          for i:=1 to NFIBERS do
               FeasaCom_UserCal_ResetxyOffsets(DevPath, i, toFlash);

          //Capture
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);

          //Calibration
          xRef := FormatDecimal(txtRefx.Text);
          yRef := FormatDecimal(txtRefy.Text);
          txtLog.Text := txtLog.Text + 'xRef=' + FloatToStr(xRef) + ', yRef=' + FloatToStr(yRef) + #13#10;
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_AdjustxyOffsets(DevPath, i, xRef, yRef, toFlash);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
          end;

          //Check results
          FeasaCom_Capture(DevPath, isPWM, CaptureRange, PWM_FRAMES);
          resp := FeasaCom_Send(DevPath,'GETxyALL', ComResponse);
          if resp=-1 then
          begin
              MessageDlg('Error: unable to send the command!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end
          else if resp=0 then
          begin
              MessageDlg('Timeout detected!', mtWarning, [mbOK], 0);
              FeasaCom_Close(DevPath);
              Freemem(ComResponse);
              Abort;
          end;
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;


     Freemem(ComResponse);
end;

procedure TfrmMain.btnReadParamsClick(Sender: TObject);
var
i:integer;
DevPath:string;
NFIBERS:integer;
ComResponse:PChar;
resp:integer;
Factor:Integer;
xOffset:Single;
yOffset:Single;
WavelengthOffset:integer;
AbsIntFactor:Double;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(ComResponse, 255);

     //Read settings
     NFIBERS := numFibers.Value;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //-------------------------------------------------
          // xy OFFSETS ADJUSTMENT
          //-------------------------------------------------

          //Retrieve Intensity gains
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_GetIntensityGain(DevPath, i, @Factor);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
               txtLog.Text := txtLog.Text + 'Int Gain ' + Format('%.02d', [i]) + ': ' + IntToStr(Factor) + #13#10;
          end;

          //Retrieve xy Offsets
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_GetxyOffsets(DevPath, i, @xOffset, @yOffset);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
               txtLog.Text := txtLog.Text + 'xy Offsets ' + Format('%.02d', [i]) + ': ' + FloatToStr(xOffset) + '; ' + FloatToStr(xOffset) + #13#10;
          end;

          //Retrieve Wavelength Offsets
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_GetWavelengthOffset(DevPath, i, @WavelengthOffset);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
               txtLog.Text := txtLog.Text + 'Wl Offsets ' + Format('%.02d', [i]) + ': ' + IntToStr(WavelengthOffset) + #13#10;
          end;

          //Retrieve Abs Int Factor
          for i:=1 to NFIBERS do
          begin
               resp := FeasaCom_UserCal_GetAbsIntFactor(DevPath, i, @AbsIntFactor);
               if not resp=1 then
               begin
                    FeasaCom_GetError_Description(ComResponse);
                    MessageDlg('Error:' + ComResponse, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Freemem(ComResponse);
                    Abort;
               end;
               txtLog.Text := txtLog.Text + 'Abs Int Factor ' + Format('%.02d', [i]) + ': ' + FloatToStr(AbsIntFactor) + #13#10;
          end;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;


     Freemem(ComResponse);
end;

function TfrmMain.FormatDecimal(TxtNumber:string):double;
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

initialization
  {$I form1.lrs}

end.

