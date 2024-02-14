{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: getRGBI
*
*  DESCRIPTION: This example demonstrates how to perform a
*  capture from the Feasa LED Analyser and retrieve the RGBI
*  color data for all the fibers, extracting each one of the
*  data values from the string received and displaying them
*  on a formatted table
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
    cmdRead: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    StringGrid1: TStringGrid;
    txtBlue0: TEdit;
    txtInt0: TEdit;
    txtRed0: TEdit;
    Label11: TLabel;
    lstFibers: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label9: TLabel;
    lstPort: TComboBox;
    txtGreen0: TEdit;
    procedure cmdCaptureClick(Sender: TObject);
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

     //Open port
     if FeasaCom_Open(DevPath,57600)=1 then
     begin

          //Send command to the Led Analyser
          if not FeasaCom_Send(DevPath, 'C', ComResponse)=1 then
          begin
               MessageDlg('Unable To Communicate with the Led Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(DevPath);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;
          MessageDlg('Captured sucessfully!',mtInformation,[mbOK],0);

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

     //Clean results on Grid
     StringGrid1.Clean();

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

