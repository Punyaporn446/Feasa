{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: Sequence (blinking LED)
*
*  DESCRIPTION: This example demonstrates how to use Sequence
*  functions provided in the SO Library to test a blinking LED
*  so that the light pattern could be tracked.
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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, Dos, Feasa;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ChartCIE_Datay: TLineSeries;
    ChartCIE_Datax: TLineSeries;
    ChartIntensity: TChart;
    ChartCIE: TChart;
    ChartIntensity_Data: TLineSeries;
    chkTimeResImportant: TCheckBox;
    cmdFindParameters: TButton;
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
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lstPort: TComboBox;
    lstBlinkingSpeed: TComboBox;
    lstSignalSpeed: TComboBox;
    numCycles: TSpinEdit;
    numCaptureTime: TSpinEdit;
    numFiber: TSpinEdit;
    numSampleCount: TSpinEdit;
    numLEDCount: TSpinEdit;
    numFiberToTest: TSpinEdit;
    numWaitTime: TSpinEdit;
    numStartDelay: TSpinEdit;
    procedure cmdFindParametersClick(Sender: TObject);
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
     if lstPort.Items.Count > 0 then lstPort.ItemIndex := lstPort.Items.Count - 1;

     //Add options to list
     lstSignalSpeed.Items.Clear();
     lstSignalSpeed.Items.Add('VERY LOW (<1Hz)');
     lstSignalSpeed.Items.Add('LOW (1-3Hz)');
     lstSignalSpeed.Items.Add('MEDIUM (3-10Hz)');
     lstSignalSpeed.Items.Add('MODERATE (10-20Hz)');
     lstSignalSpeed.Items.Add('HIGH (20-40Hz)');
     lstSignalSpeed.Items.Add('VERY HIGH (>40Hz)');
     lstSignalSpeed.ItemIndex := 3;

     lstBlinkingSpeed.Items.Clear();
     lstBlinkingSpeed.Items.Add('0: VERY LOW');
     lstBlinkingSpeed.Items.Add('1: VERY LOW');
     lstBlinkingSpeed.Items.Add('2: LOW');
     lstBlinkingSpeed.Items.Add('3: LOW');
     lstBlinkingSpeed.Items.Add('4: MEDIUM');
     lstBlinkingSpeed.Items.Add('5: MEDIUM');
     lstBlinkingSpeed.Items.Add('6: MODERATE (fast blinking)');
     lstBlinkingSpeed.Items.Add('7: MODERATE (very fast blinking)');
     lstBlinkingSpeed.Items.Add('8: HIGH (can barely see it)');
     lstBlinkingSpeed.Items.Add('9: HIGH');
     lstBlinkingSpeed.Items.Add('10: VERY HIGH (can not see it)');
     lstBlinkingSpeed.ItemIndex := 6;

end;


procedure TfrmMain.cmdFindParametersClick(Sender: TObject);
var
DevicePath: string;
SignalSpeed: Integer;
BlinkingSpeed: Integer;
MinCycleCount: Integer;
TimeResolutionIsImportant: Integer;
TotalLEDCount: Integer;
Fiber: Integer;
CaptureTime: Integer;
WaitTime: Integer;
SampleCount: Integer;
buffer: PAnsiChar;
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
     DevicePath := lstPort.Items[lstPort.ItemIndex];

     //Retrieve parameters
     SignalSpeed := lstSignalSpeed.ItemIndex * 2;
     BlinkingSpeed := lstBlinkingSpeed.ItemIndex;
     MinCycleCount := numCycles.Value;
     TimeResolutionIsImportant := 0;
     if chkTimeResImportant.Checked then TimeResolutionIsImportant := 1;
     TotalLEDCount := numLEDCount.Value;
     Fiber := numFiberToTest.Value;
     CaptureTime := 0;
     WaitTime := 0;
     SampleCount := 0;

     //Increase maximum timeout to avoid errors caused by long captures
     FeasaCom_SetResponseTimeout(8000); //8000ms

     //Open port
     if FeasaCom_Open(DevicePath, 0)=1 then
     begin
           // Find out test settings
          if not (FeasaCom_Sequence_FindTestSettings(DevicePath, TotalLEDCount, Fiber, SignalSpeed, BlinkingSpeed, MinCycleCount, TimeResolutionIsImportant, @CaptureTime, @WaitTime, @SampleCount)=1) then
          begin
               FeasaCom_GetError_Description(PAnsiChar(buffer));
               MessageDlg(buffer, mtWarning, [mbOK], 0);
               FeasaCom_Close(DevicePath);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;

          numCaptureTime.Value := CaptureTime;
          numWaitTime.Value := WaitTime;
          numSampleCount.Value := SampleCount;

          //Close the port
          FeasaCom_Close(DevicePath);

          MessageDlg('Parameters calculated successfully', mtInformation, [mbOK], 0);
     end
     else
     begin
         MessageDlg('Unable to open port', mtWarning, [mbOK], 0);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default
end;

procedure TfrmMain.cmdSequenceTestClick(Sender: TObject);
var
resp: Integer;
i: Integer;
DevicePath: string;
Fiber: Integer;
StartDelay: Integer;
CaptureTime: Integer;
WaitTime: Integer;
SampleCount: Integer;
xValues: array[0..9999] of Single;
yValues: array[0..9999] of Single;
IntensityValues: array[0..9999] of Integer;
buffer: PAnsiChar;
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
     DevicePath := lstPort.Items[lstPort.ItemIndex];

     //Retrieve parameters
     StartDelay := numStartDelay.Value;
     Fiber := numFiber.Value;
     CaptureTime := numCaptureTime.Value;
     WaitTime := numWaitTime.Value;
     SampleCount := numSampleCount.Value;

     //Increase maximum timeout to avoid errors caused by long captures
     FeasaCom_SetResponseTimeout(8000); //8000ms

     //Open port
     if FeasaCom_Open(DevicePath, 0)=1 then
     begin
          // Find out test settings
          resp := FeasaCom_Sequence_Setup(DevicePath, StartDelay, CaptureTime, WaitTime, SampleCount, TOFLASH);
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePath);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          // Perform sequence capture
          resp := FeasaCom_Sequence_Capture(DevicePath, Fiber);
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePath);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          // Read back results
          //SetLength(xValues, SampleCount + 1);
          //SetLength(yValues, SampleCount + 1);
          //SetLength(IntensityValues, SampleCount + 1);
          resp := FeasaCom_Sequence_ReadxyI(DevicePath, Fiber, @xValues, @yValues, @IntensityValues);
          if not (resp = 1) then
          begin
             FeasaCom_GetError_Description(PAnsiChar(buffer));
             MessageDlg(buffer, mtWarning, [mbOK], 0);
             FeasaCom_Close(DevicePath);
             screen.Cursor := crDefault; //Change screen cursor to default
             Abort;
          end;

          //Clear graph
          ChartIntensity_Data.Clear;
          ChartIntensity_Data.SeriesColor := clBlack;
          ChartCIE_Datax.Clear;
          ChartCIE_Datax.SeriesColor := clRed;
          ChartCIE_Datay.Clear;
          ChartCIE_Datay.SeriesColor := clBlue;

          //Plot data to charts
          for i:=0 to SampleCount - 1 do
          begin
             ChartIntensity_Data.AddXY(i, IntensityValues[i]);
             ChartCIE_Datax.AddXY(i, xValues[i]);
             ChartCIE_Datay.AddXY(i, yValues[i]);
          end;

          //Close the port
          FeasaCom_Close(DevicePath);

          MessageDlg('Parameters calculated successfully', mtInformation, [mbOK], 0);
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

