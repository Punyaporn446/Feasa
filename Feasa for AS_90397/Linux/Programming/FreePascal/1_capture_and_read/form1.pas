{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture and Read
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser, perform a
*  measurement and download or read back the results
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
**************************************************************}

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
    Label5: TLabel;
    txtFiber: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    lstPort: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    txtLog: TEdit;
    procedure cmdCaptureAndReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 


  function FeasaCom_Open(DevPath: AnsiString; Baudrate:integer):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Close(DevPath: AnsiString):Integer cdecl; external 'libfeasacom.so';
  function FeasaCom_Send(DevPath: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer cdecl; external 'libfeasacom.so';

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
     for i:=0 to 10 do
     begin
          lstPort.Items.Add('/dev/ttyS' + IntToStr(i));
     end;
     for i:=0 to 10 do
     begin
          lstPort.Items.Add('/dev/ttyUSB' + IntToStr(i));
     end;
     lstPort.ItemIndex:=1;
end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
portPath:AnsiString;
ComResponse:PChar;
aux:string;
begin

     //Clear the Results box
     txtLog.Clear;

     //Set the port number
     portPath := lstPort.Items[lstPort.ItemIndex];

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
     if FeasaCom_Open(portPath, 57600)=1 then
     begin

          //Send command to the Led Analyser
          FeasaCom_Send(portPath,'C',ComResponse);

          //Shows the received data in the screen
          txtLog.Text := txtLog.Text + ComResponse + ',';

          //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
          aux := RightStr('00' + txtFiber.Text,2);
          //Feasa Library: Send command (Get RGBI data)
          FeasaCom_Send(portPath,'GETRGBI' + aux,ComResponse);

          //Shows the received data in the screen
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_Close(portPath);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;
end;

initialization
  {$I form1.lrs}

end.

