{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (sweeping light)
*
*  DESCRIPTION: This example demonstrates how To use Sequence
*  functions provided In the DLL To test a sweeping light
*  pattern from different LEDs, extracting intensity and
*  pattern times afterwards.
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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, Grids, Feasa;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ChartIntensity: TChart;
    ChartIntensityLineSeries1: TLineSeries;
    chkisOffToOnPattern: TCheckBox;
    cmdSequenceTest: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lstPort: TComboBox;
    numCaptureTime: TSpinEdit;
    numFibers: TSpinEdit;
    numSampleCount: TSpinEdit;
    numWaitTime: TSpinEdit;
    numStartDelay: TSpinEdit;
    gridTimes: TStringGrid;
    procedure cmdSequenceTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  TOFLASH = 0;

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

     //List available ports
     for i:=1 to 100 do
     begin
         if FeasaCom_IsPortAvailable(i)=1 then
             lstPort.Items.Add(IntToStr(i));
     end;
     if lstPort.Items.Count > 0 then lstPort.ItemIndex := lstPort.Items.Count - 1;

end;


procedure TfrmMain.cmdSequenceTestClick(Sender: TObject);
var
resp: Integer;
i: Integer;
f: Integer;
DevicePort: Integer;
FibersToTest: Integer;
StartDelay: Integer;
CaptureTime: Integer;
WaitTime: Integer;
SampleCount: Integer;
isOffToOnPattern: Integer;
IntensityValues: array[0..9999] of Integer;
buffer: PAnsiChar;
ChartSeries: array[0..20] of TLineSeries;
IntOffset: double;
MaxInt: Integer;
LowTimes: array[0..20] of Integer;
HighTimes: array[0..20] of Integer;
tIntensityValues: array[0..20] of Integer;
begin
     //Allocates memory for the response:
     //Variable buffer needs to be initialized in order to reserve
     //some memory space to save the resulting string read from the
     //LED Analyser
     GetMem(buffer, 255);

     if lstPort.Items.Count=0 then
     begin
         MessageDlg('No devices detected', mtWarning, [mbOK], 0);
         Abort;
     end;
     if lstPort.ItemIndex=-1 then
     begin
         MessageDlg('Please, select a port first', mtWarning, [mbOK], 0);
         Abort;
     end;

     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

     //Set the port number
     DevicePort := StrToInt(lstPort.Items[lstPort.ItemIndex]);

     //Retrieve parameters
     StartDelay := numStartDelay.Value;
     FibersToTest := numFibers.Value;
     CaptureTime := numCaptureTime.Value;
     WaitTime := numWaitTime.Value;
     SampleCount := numSampleCount.Value;
     isOffToOnPattern := 0;
     if chkisOffToOnPattern.Checked then isOffToOnPattern := 1;

     //Increase maximum timeout to avoid errors caused by long captures
     FeasaCom_SetResponseTimeout(8000); //8000ms

     //Open port
     if FeasaCom_Open(DevicePort, '57600')=1 then
     begin
          // Find out test settings
          resp := FeasaCom_Sequence_Setup(DevicePort, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH);
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePort);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          // Perform sequence capture
          resp := FeasaCom_Sequence_Capture(DevicePort, 0); //0: test all fibers
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePort);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          //Clear graph
          ChartIntensity.ClearSeries();

          // Read back results
          IntOffset := 0;
          MaxInt := 0;
          for f:=1 to FibersToTest do
          begin
              // Add series
              ChartSeries[f-1] := TLineSeries.Create(self);
              ChartIntensity.AddSeries(ChartSeries[f-1]);
              // Read back intensity values
              resp := FeasaCom_Sequence_ReadIntensity(DevicePort, f, @IntensityValues);
              if not (resp = 1) then
              begin
                 FeasaCom_GetError_Description(PAnsiChar(buffer));
                 MessageDlg(buffer, mtWarning, [mbOK], 0);
                 FeasaCom_Close(DevicePort);
                 screen.Cursor := crDefault; //Change screen cursor to default
                 Abort;
              end;
              //Plot data to graph
              ChartSeries[f-1].Clear;
              ChartSeries[f-1].SeriesColor := clRed;
              for i:=0 to SampleCount - 1 do
              begin
                  ChartSeries[f-1].AddXY(i, IntensityValues[i] + IntOffset);
                  if IntensityValues[i]>MaxInt then MaxInt := IntensityValues[i];
              end;
              // Calculates new offset
              IntOffset := IntOffset + MaxInt * 1.1;
          end;

          // Retrieve LED times
          resp := FeasaCom_Sequence_GetSweepingPattern(DevicePort, FibersToTest, isOffToOnPattern, @LowTimes, @HighTimes, @tIntensityValues); //0: test all fibers
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePort);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          // Print times in grid
          gridTimes.ClearRows;
          gridTimes.RowCount := FibersToTest + 1;
          gridTimes.FixedRows := 1;
          gridTimes.Rows[0].Strings[0] := 'Fib';
          gridTimes.Rows[0].Strings[1] := 'Low (ms)';
          gridTimes.Rows[0].Strings[2] := 'High (ms)';
          gridTimes.Rows[0].Strings[3] := 'Intensity';
          for i:=0 to FibersToTest - 1 do
          begin
              gridTimes.Rows[i + 1].Strings[0] := IntToStr(i + 1);
              gridTimes.Rows[i + 1].Strings[1] := IntToStr(LowTimes[i]);
              gridTimes.Rows[i + 1].Strings[2] := IntToStr(HighTimes[i]);
              gridTimes.Rows[i + 1].Strings[3] := IntToStr(tIntensityValues[i]);
          end;

          //Close the port
          FeasaCom_Close(DevicePort);
     end
     else
     begin
         MessageDlg('Unable to open port', mtWarning, [mbOK], 0);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default
end;


initialization
  {$I form1.lrs}

end.

