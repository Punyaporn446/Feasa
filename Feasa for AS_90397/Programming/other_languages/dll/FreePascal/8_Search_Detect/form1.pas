{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Search & Detect
*
*  DESCRIPTION: This example demonstrates how to list
*  all available Feasa devices, and also to locate
*  the port number of a connected Device based on its serial
*  number.
*
*  This example uses a dynamic library to access to the
*  functions related to the LED Analyser. This library is read
*  in runtime so you have to be sure that the file feasacom64.dll
*  exists in the same location of the EXE or in windows/system32
*  folder, however some compillers allow to reference the DLL
*  library from alternative locations using absolute or relative
*  paths.
*
*  Note: there are 32 and 64-bit versions of the DLL, so one or
*  the other has to be used depending on the compiler/IDE platform
*  or binary target platform.
*
***************************************************************}

unit form1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynHighlighterAny;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cmdCaptureAndRead: TButton;
    cmdDetect: TButton;
    cmdDetectSerials: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblLog: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtSN: TEdit;
    procedure cmdCaptureAndReadClick(Sender: TObject);
    procedure cmdDetectClick(Sender: TObject);
    procedure cmdDetectSerialsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 
                                                 
  function FeasaCom_EnumPorts():Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_IsConnected(SerialNumber:AnsiString; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SetResponseTimeout(Timeout:LongWord):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Open(CommPort: integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Close(CommPort: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Send(CommPort: integer; Command: AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Detect(CommPorts:PInteger; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_DetectSN(SerialNumbers:PPAnsiChar; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
portNumber:integer;
ComResponse:PChar;
SerialNumber: string;
begin

     //Check the serial number
     if Length(txtSN.Text)<4 then
     begin
          MessageDlg('Bad Serial Number', mtInformation, [mbOK], 0);
          txtSN.SetFocus;
          Abort;
     end;

     //Set the port serial number
     SerialNumber := RightStr('0000' + txtSN.Text, 4);

     //Clear the Results box
     lblLog.Caption := '';

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Find out if the LED Analyser is connected
     portNumber := FeasaCom_IsConnected(SerialNumber, '57600');
     if portNumber=-1 then
     begin
          MessageDlg('Unable to find the LED Analyser with SN:' + SerialNumber, mtInformation, [mbOK], 0);
          Abort;
     end
     else
     begin
         lblLog.Caption := 'LED Analyser found on port ' + IntToStr(portNumber) + chr(13) + chr(10);
     end;

     //Open port
     if FeasaCom_Open(portNumber,'57600')=1 then
     begin
          //Allocates memory for the response
          GetMem(ComResponse, 2048); //CAUTION! we need more buffer in this example due to longer responses

          //DLL: Send command (GETSTATUS)
          FeasaCom_Send(portNumber,'GETSTATUS',ComResponse);

          //Shows the received data in the screen
          lblLog.Caption := lblLog.Caption + ComResponse;

          //Close the port
          FeasaCom_Close(portNumber);
     end;
end;

procedure TfrmMain.cmdDetectClick(Sender: TObject);
var
i: Integer;
nDetected: Integer;
Ports: array of Integer;
begin
     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     FeasaCom_EnumPorts();

     //Initialize varaibles
     SetLength(Ports, 255);

     //Clear the Results box
     lblLog.Caption := '';

     //Perform detection
     nDetected := FeasaCom_Detect(PInteger(Ports), 'AUTO');
     if nDetected>0 then
     begin
         lblLog.Caption := lblLog.Caption + 'Devices detected:' + #13 + #10;
         for i:=0 to nDetected-1 do
         begin
             lblLog.Caption := lblLog.Caption + '...on port COM' + IntToStr(Ports[i]) + #13 + #10;
         end;
     end
     else begin
         MessageDlg('No devices detected!', mtInformation, [mbOK], 0);
     end;
end;

procedure TfrmMain.cmdDetectSerialsClick(Sender: TObject);
var
i: Integer;
nDetected: Integer;
lstSerials: array of PAnsiChar;
begin
     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     FeasaCom_EnumPorts();

     //Initialize varaibles
     SetLength(lstSerials, 50);
     for i := 0 to 49 do
         GetMem(lstSerials[i], 10);

     //Clear the Results box
     lblLog.Caption := '';

     //Perform detection
     nDetected := FeasaCom_DetectSN(PPAnsiChar(lstSerials), 'AUTO');
     if nDetected>0 then
     begin
         lblLog.Caption := lblLog.Caption + 'Devices detected:' + #13 + #10;
         for i:=0 to nDetected-1 do
         begin
             lblLog.Caption := lblLog.Caption + '...SN ' + lstSerials[i] + #13 + #10;
         end;
     end
     else begin
         MessageDlg('No devices detected!', mtInformation, [mbOK], 0);
     end;
end;

initialization
  {$I form1.lrs}

end.

