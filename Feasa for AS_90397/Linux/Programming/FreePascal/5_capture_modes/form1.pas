{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: capture_modes
*
*  DESCRIPTION: This example demonstrates the different methods
*  available for performing measurements (captures) on the
*  LED Analyser. Then, the responses received are parsed and
*  the numerical values are exatracted and printed to a
*  grid-style output.
*  This example uses a helper function to format the received
*  decimal string to the default Local Decimal character.
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
  StdCtrls, ExtCtrls, strutils, Grids;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cmdCapture: TButton;
    cmdCaptureEasy: TButton;
    cmdRead: TButton;
    Label10: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lstCaptureMode: TComboBox;
    lstCaptureRange: TComboBox;
    lstCaptureFrames: TComboBox;
    StringGrid1: TStringGrid;
    lstFibers: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label9: TLabel;
    lstPort: TComboBox;
    procedure cmdCaptureClick(Sender: TObject);
    procedure cmdCaptureEasyClick(Sender: TObject);
    procedure cmdReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  function FeasaCom_Open(DevPath: AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Close(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Send(DevPath: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_IsPortAvailable(DevPath:AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_SetResponseTimeout(Timeout:LongWord):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Capture(DevPath: AnsiString; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.so';
  procedure FeasaCom_GetError_Description(ResponseText:PAnsiChar) cdecl; external 'libfeasacom.so';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
//variables
var
i:integer;
begin
     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Add the port numbers to the list
     for i:=0 to 30 do
     begin
         if FeasaCom_IsPortAvailable('/dev/ttyS' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyS' + IntToStr(i));
     end;
     for i:=0 to 30 do
     begin
         if FeasaCom_IsPortAvailable('/dev/ttyUSB' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyUSB' + IntToStr(i));
     end;
     lstPort.ItemIndex:=0;

     //Add number of fibers
     lstFibers.Items.Add('20');
     lstFibers.Items.Add('10');
     lstFibers.Items.Add('3');
     lstFibers.Items.Add('2');
     lstFibers.ItemIndex:=0;

     //Fill the list with the Capture Modes
     lstCaptureMode.Items.Add('AUTO');
     lstCaptureMode.Items.Add('MANUAL');
     lstCaptureMode.Items.Add('PWM: AUTO-RANGE & AUTO-FRAMING');
     lstCaptureMode.Items.Add('PWM: MANUAL-RANGE & AUTO-FRAMING');
     lstCaptureMode.Items.Add('PWM: AUTO-RANGE & MANUAL-FRAMING');
     lstCaptureMode.Items.Add('PWM: MANUAL-RANGE & MANUAL-FRAMING');
     lstCaptureMode.ItemIndex:=0;

     //Fill the list with the Capture Ranges
     lstCaptureRange.Items.Add('LOW');
     lstCaptureRange.Items.Add('MEDIUM');
     lstCaptureRange.Items.Add('HIGH');
     lstCaptureRange.Items.Add('SUPER');
     lstCaptureRange.Items.Add('ULTRA');
     lstCaptureRange.ItemIndex:=2;

     //Fill the list with the Capture Frames
     for i:=1 to 15 do
     begin
         lstCaptureFrames.Items.Add(IntToStr(i));
     end;
     lstCaptureFrames.ItemIndex:=4;

     //Setup Grid
     StringGrid1.Columns.Items[0].Title.Caption:='Red';
     StringGrid1.Columns.Items[1].Title.Caption:='Green';
     StringGrid1.Columns.Items[2].Title.Caption:='Blue';
     StringGrid1.Columns.Items[3].Title.Caption:='Intensity';

end;

procedure TfrmMain.cmdCaptureClick(Sender: TObject);
var
DevPath:AnsiString;
ComResponse:PChar;
CaptureCommand:String;
begin

     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

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

     //Increase the maximum timeout in order to avoid timeout events caused by long captures (manual, PWM...)
     FeasaCom_SetResponseTimeout(8000); //8000 milliseconds

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //Compose the Capture Command
          if lstCaptureMode.ItemIndex=0 then begin
               CaptureCommand := 'CAPTURE';
          end else if lstCaptureMode.ItemIndex=1 then begin
               CaptureCommand := 'CAPTURE' + IntToStr(lstCaptureRange.ItemIndex + 1);
          end else if lstCaptureMode.ItemIndex=2 then begin
               CaptureCommand := 'CAPTUREPWM'
          end else if lstCaptureMode.ItemIndex=3 then begin
               CaptureCommand := 'CAPTURE' + IntToStr(lstCaptureRange.ItemIndex + 1) + 'PWM';
          end else if lstCaptureMode.ItemIndex=4 then begin
               CaptureCommand := 'CAPTUREPWM' + RightStr('00' + IntToStr(lstCaptureFrames.ItemIndex + 1),2);
          end else if lstCaptureMode.ItemIndex=5 then begin
               CaptureCommand := 'CAPTURE' + IntToStr(lstCaptureRange.ItemIndex + 1) + 'PWM' + RightStr('00' + IntToStr(lstCaptureFrames.ItemIndex + 1),2);
          end;

          //Send command to the Led Analyser
          if not FeasaCom_Send(DevPath, CaptureCommand, ComResponse)=1 then
          begin
               MessageDlg('Unable To Communicate with the Led Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;
          MessageDlg('Capture Done!',mtInformation,[mbOK],0);

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
         MessageDlg('Unable to open Port ' + DevPath, mtWarning, [mbOK], 0);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default
end;

procedure TfrmMain.cmdCaptureEasyClick(Sender: TObject);
var
DevPath:AnsiString;
err:PChar;
isPWM:integer;
CaptureRange:integer;
CapturePWMFrames:integer;
r:integer;
begin
     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the error description
     GetMem(err, 255);

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //Compose the Capture Command
          if lstCaptureMode.ItemIndex=0 then begin
               isPWM := 0;
               CaptureRange := 0;
          end else if lstCaptureMode.ItemIndex=1 then begin
               isPWM := 0;
               CaptureRange := lstCaptureRange.ItemIndex + 1;
          end else if lstCaptureMode.ItemIndex=2 then begin
               isPWM := 1;
               CaptureRange := 0;
               CapturePWMFrames := 0;
          end else if lstCaptureMode.ItemIndex=3 then begin
               isPWM := 1;
               CaptureRange := lstCaptureRange.ItemIndex + 1;
               CapturePWMFrames := 0;
          end else if lstCaptureMode.ItemIndex=4 then begin
               isPWM := 1;
               CaptureRange := 0;
               CapturePWMFrames := lstCaptureFrames.ItemIndex + 1;
          end else if lstCaptureMode.ItemIndex=5 then begin
               isPWM := 1;
               CapturePWMFrames := lstCaptureFrames.ItemIndex + 1;
               CaptureRange := lstCaptureRange.ItemIndex + 1;
          end;

          //Send command to the Led Analyser
          r := FeasaCom_Capture(DevPath, isPWM, CaptureRange, CapturePWMFrames);
          if r=-1 then
          begin
               FeasaCom_GetError_Description(err);
               MessageDlg('Unable to Communicate with the Led Analyser: ' + err,mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end
          else if r=0 then
          begin
               MessageDlg('Timeout or Syntax error',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;

          MessageDlg('Capture Done!',mtInformation,[mbOK],0);

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
         MessageDlg('Unable to open Port ' + DevPath, mtWarning, [mbOK], 0);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default
end;


procedure TfrmMain.cmdReadClick(Sender: TObject);
var
DevPath:AnsiString;
ComResponse:PChar;
aux:string;
n:integer;
i:integer;
begin

     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //Get the number of fibers to read
     n := StrToInt(lstFibers.Text);

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

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //Allocates memory for the response
          GetMem(ComResponse, 255);

          for i:=1 to n do
          begin
               //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
               aux := RightStr('00' + IntToStr(i),2);
               //Send command to the Led Analyser
               if not FeasaCom_Send(DevPath, 'GETRGBI' + aux, ComResponse)=1 then
               begin
                    MessageDlg('Error occured while trying to send command: ' + 'GETRGBI' + aux, mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    screen.Cursor := crDefault; //Change screen cursor to default
                    Abort;
               end
               else
               begin
                    //Parse received data
                    StringGrid1.Rows[i].Strings[0] := MidStr(ComResponse,1,3); //Red
                    StringGrid1.Rows[i].Strings[1] := MidStr(ComResponse,5,3); //Green
                    StringGrid1.Rows[i].Strings[2] := MidStr(ComResponse,9,3); //Blue
                    StringGrid1.Rows[i].Strings[3] := MidStr(ComResponse,13,5); //Intensity
               end;
          end;

          //Refresh grid
          StringGrid1.Refresh;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
         MessageDlg('Unable to open Port ' + DevPath, mtWarning, [mbOK], 0);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default

end;


initialization
  {$I form1.lrs}

end.

