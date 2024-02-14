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

  function FeasaCom_EnumPorts():Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_IsConnected(DevPath: PAnsiChar; SerialNumber:AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_SetResponseTimeout(Timeout:LongWord):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Open(DevPath: AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Close(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Send(DevPath: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.x86_64.so';
  function FeasaCom_Detect(DevPaths: PPAnsiChar; Baudrate:Integer):Integer stdcall; external 'libfeasacom.x86_64.so';
  function FeasaCom_DetectSN(SerialNumbers:PPAnsiChar; Baudrate:Integer):Integer stdcall; external 'libfeasacom.x86_64.so';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
ComResponse:PChar;
SerialNumber: string;
DevPath:PChar;
resp: integer;
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

     //Initialize variables
     GetMem(DevPath, 255);

     //Find out if the Led Analyser is connected
     resp := FeasaCom_IsConnected(DevPath, SerialNumber, 57600);
     if resp=-1 then
     begin
          MessageDlg('Unable to find the Led Analyser with SN:' + SerialNumber, mtInformation, [mbOK], 0);
          Abort;
     end
     else
     begin
         lblLog.Caption := 'Led Analyser found on port ' + DevPath + chr(13) + chr(10);
     end;

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin
          //Allocates memory for the response
          GetMem(ComResponse, 2048); //CAUTION! we need more buffer in this example due to longer responses

          //SO Library: Send command (GETSTATUS)
          FeasaCom_Send(DevPath,'GETSTATUS',ComResponse);

          //Shows the received data in the screen
          lblLog.Caption := lblLog.Caption + ComResponse;

          //Close the port
          FeasaCom_Close(DevPath);
     end;
end;

procedure TfrmMain.cmdDetectClick(Sender: TObject);
var
i: Integer;
nDetected: Integer;
DevPaths: array of PAnsiChar;
begin
     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     FeasaCom_EnumPorts();

     //Initialize varaibles
     SetLength(DevPaths, 50);
     for i := 0 to 49 do
         GetMem(DevPaths[i], 255);

     //Clear the Results box
     lblLog.Caption := '';

     //Perform detection
     nDetected := FeasaCom_Detect(PPAnsiChar(DevPaths), 0);
     if nDetected>0 then
     begin
         lblLog.Caption := lblLog.Caption + 'Devices detected:' + #13 + #10;
         for i:=0 to nDetected-1 do
         begin
             lblLog.Caption := lblLog.Caption + '...on port ' + DevPaths[i] + #13 + #10;
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
     nDetected := FeasaCom_DetectSN(PPAnsiChar(lstSerials), 0);
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

