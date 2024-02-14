{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Fast Test (Multi-threaded)
*
*  DESCRIPTION: This example demonstrates how to use the multi-
*  threaded functions provided in the SO Library to set up a
*  fast and efficient communication schema for your application.
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
  StdCtrls, ExtCtrls, Grids, Dos, strutils, Feasa;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    cmdAdd: TButton;
    cmdCapture: TButton;
    GroupBox3: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lstPortsToTest: TListBox;
    StringGrid1: TStringGrid;
    txtBlue0: TEdit;
    txtInt0: TEdit;
    txtRed0: TEdit;
    Label11: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    lblExecutionTime: TLabel;
    lstPort: TComboBox;
    txtGreen0: TEdit;
    procedure cmdAddClick(Sender: TObject);
    procedure cmdCaptureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

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

     //Add the port numbers in the list from 0 to 16
     for i:=0 to 50 do
     begin
          if FeasaCom_IsPortAvailable('/dev/ttyS' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyS' + IntToStr(i));
          if FeasaCom_IsPortAvailable('/dev/ttyUSB' + IntToStr(i))=1 then
             lstPort.Items.Add('/dev/ttyUSB' + IntToStr(i));
     end;
     lstPort.ItemIndex:=0;

     //Setup Grid
     StringGrid1.Columns.Items[0].Title.Caption:='SN';
     StringGrid1.Columns.Items[1].Title.Caption:='Fiber';
     StringGrid1.Columns.Items[2].Title.Caption:='Hue';
     StringGrid1.Columns.Items[3].Title.Caption:='Saturation';
     StringGrid1.Columns.Items[4].Title.Caption:='Intensity';

end;

procedure TfrmMain.cmdCaptureClick(Sender: TObject);
var
Resp: Integer;
PortCount: Integer; 
DevPaths0: array of AnsiString;
DevPaths: array of PAnsiChar;
buffer: PAnsiChar;
i, j, f: Integer;
Responses: array of PAnsiChar;
ReturnValues: array of Integer;
SNs: array of AnsiString;
nLines: Integer;
tIni:Int64;
tEnd:Int64;
lstLines: TStringList;
lstData: TStringList;
begin

     tIni:=GetMsCount();

     if lstPortsToTest.Items.Count=0 then
     begin
         MessageDlg('Please, add at least one port to the list of devices to be tested.', mtWarning, [mbOK], 0);
         Abort;
     end;

     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

     //Set the port number
     PortCount := lstPortsToTest.Items.Count;

     //Initialize vars
     SetLength(DevPaths0, PortCount);
     SetLength(DevPaths, PortCount);
     SetLength(ReturnValues, PortCount);
     SetLength(Responses, PortCount);
     SetLength(SNs, PortCount);
     for i := 0 to (PortCount-1) do
     begin
          //GetMem(DevPaths[i], 255);
          DevPaths0[i] := PChar(lstPortsToTest.Items[i]);
          DevPaths[i] := PChar(DevPaths0[i]);
          ReturnValues[i] := -1;
     end;

     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //Led Analyser
     GetMem(buffer, 255);

     //Open port
     if FeasaCom_Open_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount, 57600)=1 then
     begin
           //Initialize Responses array
           SetLength(Responses, PortCount);
           InitializeArrayOfStrings(PPAnsiChar(Responses), PortCount, 1024);

           //Retrieve SNs connected
           for i:=0 to (PortCount-1) do
           begin
//                MessageDlg(DevPaths[i], mtWarning, [mbOK], 0);
                Resp := FeasaCom_GetSNByPort(buffer, DevPaths[i]);
                if (Resp=1)  then
                   SNs[i] := buffer
                else
                   SNs[i] := '';
           end;

          //Send command to All Analysers connected
          if not FeasaCom_SendToAll(PInteger(ReturnValues), 'C', PPAnsiChar(Responses))=1 then
          begin
               for i:=0 to (PortCount-1) do
               begin
                    if ReturnValues[i]=-1 then
                    begin
                         MessageDlg('Unable to send command to ' + SNs[i] + '!', mtWarning, [mbOK], 0);
                         FeasaCom_Close_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount);
                         screen.Cursor := crDefault; //Change screen cursor to default
                         Abort;
                    end
                    else
                    begin
                         MessageDlg('Timeout or Syntax error detected in ' + SNs[i] + '!', mtWarning, [mbOK], 0);
                         FeasaCom_Close_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount);
                         screen.Cursor := crDefault; //Change screen cursor to default
                         Abort;
                    end;
               end;
          end;
          //MessageDlg('Captured sucessfully!',mtInformation,[mbOK],0); 

          //Send command to All Analysers connected
          if not FeasaCom_SendToAll(PInteger(ReturnValues), 'GETHSIALL', PPAnsiChar(Responses))=1 then
          begin
               for i:=0 to (PortCount-1) do
               begin
                    if ReturnValues[i]=-1 then
                    begin
                         MessageDlg('Unable to send command to ' + SNs[i] + '!', mtWarning, [mbOK], 0);
                         FeasaCom_Close_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount);
                         screen.Cursor := crDefault; //Change screen cursor to default
                         Abort;
                    end
                    else
                    begin
                         MessageDlg('Timeout or Syntax error detected in ' + SNs[i] + '!', mtWarning, [mbOK], 0);
                         FeasaCom_Close_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount);
                         screen.Cursor := crDefault; //Change screen cursor to default
                         Abort;
                    end;
               end;
          end;

          //Clear grid
          StringGrid1.RowCount:=1;

          for i:=0 to (PortCount-1) do
          begin
               //nLines := Feasa_Parse_HSI_All(Responses[i], PSingle(HueValues), PInteger(SaturationValues), PInteger(IntensityValues));
               lstLines := TStringList.Create; 
               lstData := TStringList.Create;
               lstLines.StrictDelimiter := True;
               lstLines.Delimiter := #10;
               lstLines.DelimitedText := StringReplace(StringReplace(Responses[i], #13, '', [rfReplaceAll]),#4,'', [rfReplaceAll]);
               nLines := lstLines.Count;
               if nLines>0 then
               begin
                    for f:=0 to (nLines-1) do
                    begin
                         if Length(lstLines[f])>0 then
                         begin
                           j := StringGrid1.RowCount;
                           StringGrid1.RowCount := StringGrid1.RowCount + 1;
                           lstData.StrictDelimiter := True;
                           lstData.Delimiter := ' ';
                           lstData.DelimitedText := lstLines[f];
                           StringGrid1.Rows[j].Strings[0] := SNs[i];
                           StringGrid1.Rows[j].Strings[1] := lstData[0];
                           StringGrid1.Rows[j].Strings[2] := lstData[1];
                           StringGrid1.Rows[j].Strings[3] := lstData[2];
                           StringGrid1.Rows[j].Strings[4] := lstData[3]
                           {StringGrid1.Rows[j].Strings[0] := SNs[i];
                           StringGrid1.Rows[j].Strings[1] := AnsiMidStr(lstLines[f],1,2);
                           StringGrid1.Rows[j].Strings[2] := AnsiMidStr(lstLines[f],4,6);
                           StringGrid1.Rows[j].Strings[3] := AnsiMidStr(lstLines[f],11,3);
                           StringGrid1.Rows[j].Strings[4] := AnsiMidStr(lstLines[f],15,5);}
                         end;
                    end;
               end;
               lstLines.Free;
               lstData.Free;
          end;

          //Close the port
          FeasaCom_Close_Multi(PInteger(ReturnValues), PPAnsiChar(DevPaths), PortCount);
     end
     else
     begin
         MessageDlg('Unable to open all ports', mtWarning, [mbOK], 0);
     end;

     tEnd:=GetMsCount();
     lblExecutionTime.Caption := 'Execution Time: ' + IntToStr(tEnd - tIni) + 'ms';

     screen.Cursor := crDefault; //Change screen cursor to default
end;

procedure TfrmMain.cmdAddClick(Sender: TObject);
begin
     if lstPort.ItemIndex>=0 then
     begin
          lstPortsToTest.Items.Add(lstPort.Items[lstPort.ItemIndex]);
          lstPort.Items.Delete(lstPort.ItemIndex);
     end;
end;


initialization
  {$I form1.lrs}

end.

