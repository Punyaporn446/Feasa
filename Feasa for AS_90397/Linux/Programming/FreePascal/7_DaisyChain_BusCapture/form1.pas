{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: DaisyChain (Bus capture)
*
*  DESCRIPTION: This example demonstrates how to perform
*  a capture for all Daisy-chained analysers, through the
*  SO Library functions and then retrieve the HSI values for
*  the fiber requested.
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
  StdCtrls, ExtCtrls, strutils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cmdAdd: TButton;
    cmdCaptureAndRead: TButton;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    lblLog: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lstAnalysers: TListBox;
    lstCaptureMode: TComboBox;
    lstCaptureRange: TComboBox;
    lstPort: TComboBox;
    txtFiber: TEdit;
    procedure cmdCaptureAndReadClick(Sender: TObject);
    procedure cmdAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  function FeasaCom_EnumPorts():Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Open(DevPath: AnsiString; Baudrate:Integer):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Close(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Send(DevPath: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_IsPortAvailable(DevPath:AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_DaisyChain_Add(DevPath: AnsiString; SerialNumber: AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_DaisyChain_Capture(DevPath: AnsiString; isPWM: integer; CaptureRange: integer; CapturePWMFrames: integer):Integer cdecl; external 'libfeasacom.so';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
//variables
var
i:integer;
begin
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

     //Fill the list with the Capture Modes
     lstCaptureMode.Items.Add('AUTO');
     lstCaptureMode.Items.Add('MANUAL');
     lstCaptureMode.ItemIndex:=0;

     //Fill the list with the Capture Ranges
     lstCaptureRange.Items.Add('LOW');
     lstCaptureRange.Items.Add('MEDIUM');
     lstCaptureRange.Items.Add('HIGH');
     lstCaptureRange.Items.Add('SUPER');
     lstCaptureRange.Items.Add('ULTRA');
     lstCaptureRange.ItemIndex:=2;

end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
i:integer;
DevPath:AnsiString;
ComResponse:PChar;
CaptureRange:integer;
auxsn:string;
fib:string;
r:integer;
begin

     //Check if Any Analyser has been added to the Daisy-Chain BUS
     if lstAnalysers.Items.Count=0 then
     begin
          MessageDlg('Please, add Analysers to the Bus first.', mtInformation, [mbOK], 0);
          Abort;
     end;

     //Clear Log
     lblLog.Caption := '';

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

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin

          //Add devices to the BUS structure
          for i:=0 to (lstAnalysers.Items.Count-1) do
          begin
               if not FeasaCom_DaisyChain_Add(DevPath, lstAnalysers.Items[i])=1 then
               begin
                    MessageDlg('Unable to Add Analyser with SN:' + lstAnalysers.Items[i] + sLineBreak + 'Please, check power supply and communication cables', mtWarning, [mbOK], 0);
                    FeasaCom_Close(DevPath);
                    Abort;
               end;
          end;

          //Compose the Capture Command
          if lstCaptureMode.ItemIndex=0 then begin
               CaptureRange := 0;
          end else if lstCaptureMode.ItemIndex=1 then begin
               CaptureRange := lstCaptureRange.ItemIndex + 1;
          end;

          lblLog.Caption := lblLog.Caption + 'Capturing...'; //Log

          //Send command to the Led Analyser
          r := FeasaCom_DaisyChain_Capture(DevPath, 0, CaptureRange, 0);
          if r=-1 then
          begin
               MessageDlg('Unable to Communicate with the Led Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               Abort;
          end
          else if r=-1 then
          begin
               MessageDlg('Timeout or Syntax error',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               Abort;
          end;

          lblLog.Caption := lblLog.Caption + ' Done!' + slineBreak; //Log

          //get the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
          fib := RightStr('00' + txtFiber.Text,2);

          //Free the bus
          if not FeasaCom_Send(DevPath, 'BUSFREE', ComResponse)=1 then
          begin
               MessageDlg('Unable to free the bus!',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               Abort;
          end;

          //Retrieve Serial Number of Master/Main Analyser
          if not FeasaCom_Send(DevPath, 'GETSERIAL', ComResponse)=1 then
          begin
               MessageDlg('Unable to Communicate with the Led Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               Abort;
          end;
          auxsn := ComResponse;

          //Get Color & Intensity data for Master/Main Analyser
          if not FeasaCom_Send(DevPath, 'GETHSI' + fib, ComResponse)=1 then
          begin
               MessageDlg('Unable to Communicate with the Led Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               Abort;
          end;
          //Shows the received data in the screen
          lblLog.Caption := lblLog.Caption + 'Fib ' + fib + '(' + auxsn + '): ' + ComResponse + slineBreak;

          //Get Color & Intensity data for each Analyser attached to the BUS
          for i:=0 to (lstAnalysers.Items.Count-1) do
          begin
               //Set the bus owner
               if not FeasaCom_Send(DevPath, 'BUSGET' + lstAnalysers.Items[i], ComResponse)=1 then
               begin
                    MessageDlg('Unable to get the bus!',mtWarning,[mbOK],0);
                    FeasaCom_Close(DevPath);
                    Abort;
               end;
               //Send command to the Led Analyser
               if not FeasaCom_Send(DevPath, 'GETHSI' + fib, ComResponse)=1 then
               begin
                    MessageDlg('Unable to Communicate with the Led Analyser',mtWarning,[mbOK],0);
                    FeasaCom_Close(DevPath);
                    Abort;
               end;

               //Shows the received data in the screen
               lblLog.Caption := lblLog.Caption + 'Fib ' + fib + '(' + lstAnalysers.Items[i] + '): ' + ComResponse + slineBreak;

               //Free the bus
               if not FeasaCom_Send(DevPath, 'BUSFREE', ComResponse)=1 then
               begin
                    MessageDlg('Unable to free the bus!',mtWarning,[mbOK],0);
                    FeasaCom_Close(DevPath);
                    Abort;
               end;
          end;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open Port ' + DevPath,mtWarning,[mbOK],0);
     end;
end;

procedure TfrmMain.cmdAddClick(Sender: TObject);
var
SerialNumber:string;
begin
     //Ask for Analyser
     SerialNumber := InputBox('Add to DaisyChain', 'Please, input the Serial Number of the Analyser to Add:', '');

     //Check the serial number
     if Length(SerialNumber)<>4 then
     begin
          MessageDlg('Bad Serial Number', mtInformation, [mbOK], 0);
          Abort;
     end;

     //Add Analyser to the list
     lstAnalysers.Items.Add(SerialNumber);
end;

initialization
  {$I form1.lrs}

end.

