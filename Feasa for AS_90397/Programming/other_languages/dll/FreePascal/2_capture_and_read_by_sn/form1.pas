{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Capture And Read (by Serial Number)
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser using the SN
*  instead of the COM port; then, perform a measurement and
*  download or read back the results
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
    txtFiber: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    txtSN: TEdit;
    txtLog: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure cmdCaptureAndReadClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  function FeasaCom_OpenSN(SerialNumber: AnsiString; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_CloseSN(SerialNumber: AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_SendSN(SerialNumber: AnsiString; Command: AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }


procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
SerialNumber:string;
ComResponse:PChar;
aux:string;
begin

     //Clear the Results box
     txtLog.Clear;

     //Check the serial number
     if Length(txtSN.Text)<4 then
     begin
          MessageDlg('Bad Serial Number', mtInformation, [mbOK], 0);
          txtSN.SetFocus;
          Abort;
     end;

     //Set the port serial number
     SerialNumber := txtSN.Text;

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //LED Analyser
     GetMem(ComResponse, 255);

     //Open port
     if FeasaCom_OpenSN(SerialNumber,'57600')=1 then
     begin

          //Send command to the LED Analyser
          FeasaCom_SendSN(SerialNumber,'C',ComResponse);

          //Shows the received data in the screen
          txtLog.Text := txtLog.Text + ComResponse + ',';

          //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
          aux := RightStr('00' + txtFiber.Text,2);
          //DLL: Send command (Get RGBI data)
          FeasaCom_SendSN(SerialNumber,'GETRGBI' + aux,ComResponse);

          //Shows the received data in the screen
          txtLog.Text := txtLog.Text + ComResponse;

          //Close the port
          FeasaCom_CloseSN(SerialNumber);
     end
     else
     begin
          MessageDlg('Unable to open the port', mtInformation, [mbOK], 0);
     end;
end;

initialization
  {$I form1.lrs}

end.

