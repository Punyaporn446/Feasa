{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: baudrate
*
*  DESCRIPTION: This example demonstrates how to establish
*  a communication with the Feasa LED Analyser picking a
*  known baudrate or detecting it automatically.
*
*  Important Note: it is not possible to communicate to a
*  Led Analyser that does not have axactly the same
*  baudrate used to open the port.
*  Factory default: 57600 baud.
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
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblLog: TLabel;
    lstPort: TComboBox;
    lstBaudrate: TComboBox;
    optBaud0: TRadioButton;
    optBaud1: TRadioButton;
    txtFiber: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Panel2: TPanel;
    procedure optBaud0Change(Sender: TObject);
    procedure optBaud1Change(Sender: TObject);
    procedure cmdCaptureAndReadClick(Sender: TObject);
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
  function FeasaCom_GetBaudrate(DevPath: AnsiString):LongInt cdecl; external 'libfeasacom.so';

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
     lstPort.ItemIndex:=1;

     //Add the possible baudrates to the list
     lstBaudrate.Items.Add('9600');
     lstBaudrate.Items.Add('19200');
     lstBaudrate.Items.Add('38400');
     lstBaudrate.Items.Add('57600');
     lstBaudrate.Items.Add('115200');
     lstBaudrate.Items.Add('230400');
     lstBaudrate.Items.Add('460800');
     lstBaudrate.Items.Add('921600');
     lstBaudrate.ItemIndex := 3;
end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
DevPath:AnsiString;
ComResponse:PChar;
Baudrate:Integer;
aux:string;
tini:TTimeStamp;
tdif:comp;
begin

     //set hourglass cursor
     frmMain.Cursor := crHourGlass;

     //Get timestamp
     tini := DateTimeToTimeStamp(time);

     //Clear the Results box
     lblLog.Caption:='';

     //Set the port number
     DevPath := lstPort.Items[lstPort.ItemIndex];

     //Get the baudrate
     if optBaud0.Checked=true then
     begin
        Baudrate := 0;
     end
     else
        Baudrate := StrToInt(lstBaudrate.Text);

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
     if FeasaCom_Open(DevPath,Baudrate)=1 then
     begin

          //Send command to the Led Analyser
          FeasaCom_Send(DevPath,'C',ComResponse);

          //Shows the received data in the screen
          lblLog.Caption := lblLog.Caption + ComResponse + chr(13) + chr(10);

          //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
          aux := RightStr('00' + txtFiber.Text,2);
          //Feasa Library: Send command (Capture)
          FeasaCom_Send(DevPath,'GETRGBI' + aux,ComResponse);

          //Shows the received data in the screen
          lblLog.Caption := lblLog.Caption + ComResponse + chr(13) + chr(10);

          //Shows the execution time
          tdif := TimeStampToMSecs(DateTimeToTimeStamp(time)) - TimeStampToMSecs(tini);
          lblLog.Caption := lblLog.Caption + 'Execution time: ' + FloatToStr(tdif);

          //Shows detected baudrate
          if optBaud0.Checked=true then
          begin
                    lblLog.Caption := lblLog.Caption + chr(13) + chr(10) + 'Baudrate: ' + IntToStr(FeasaCom_GetBaudrate(DevPath));
          end;

          //Close the port
          FeasaCom_Close(DevPath);
     end
     else
     begin
          MessageDlg('Unable to open Port ' + DevPath, mtWarning, [mbOK], 0);
     end;

     //set hourglass cursor
     frmMain.Cursor := crDefault;
end;

procedure TfrmMain.optBaud0Change(Sender: TObject);
begin
  lstBaudrate.Enabled:=false;
end;

procedure TfrmMain.optBaud1Change(Sender: TObject);
begin
  lstBaudrate.Enabled:=true;
end;

initialization
  {$I form1.lrs}

end.

