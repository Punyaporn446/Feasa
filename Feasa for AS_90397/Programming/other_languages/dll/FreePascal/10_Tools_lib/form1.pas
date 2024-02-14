{**************************************************************
*
*  (c) Feasa Enterprises Ltd
*  LED Analyser examples
*  Developed by: Carles Martínez Rius
*
*  PROJECT: tools_lib
*
*  DESCRIPTION: This example demonstrates how to use the
*  library feasa_tools64.dll or its 32-bit equivalent
*  feasa_tools.dll, to extract or parse the strings returned
*  by the Feasa LED Analyser responses and convert them
*  into usable numerical values.
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
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label5: TLabel;
    txtFiber: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    txtRed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    lstPort: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    txtGreen: TEdit;
    txtBlue: TEdit;
    txtHue: TEdit;
    txtSat: TEdit;
    txtInt: TEdit;
    procedure cmdCaptureAndReadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  function FeasaCom_Open(CommPort: integer; Baudrate:AnsiString):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Close(CommPort: integer):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_Send(CommPort: integer; Command: AnsiString; ResponseText:PAnsiChar):Integer stdcall; external 'feasacom64.dll';
  function FeasaCom_ListPortsDetected(ListOfPortsDetected:PInteger):Integer stdcall; external 'feasacom64.dll';
  function Feasa_Parse_RGBI(AnalyserResponse: PChar; Red: PByte; Green: PByte; Blue: PByte; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';
  function Feasa_Parse_HSI(AnalyserResponse: PChar; Hue: PSingle; Saturation: PInteger; Intensity:PInteger):Integer stdcall; external 'feasa_tools64.dll';

var
  frmMain: TfrmMain;

implementation

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
//variables
var
i:Integer;
PortsList:Array[0..100] of Integer;
nPorts:Integer;
begin

     //This command enumerates the existing ports to find out what are the serial
     // ports available on your computer and the devices connected to them.
     // This function needs to be executed any time that a Feasa device is
     // pluged or unpluged, while the application is running
     //FeasaCom_EnumPorts();

     //Retrieve the list of serial ports detected by the DLL
     nPorts := FeasaCom_ListPortsDetected(@PortsList[0]);
     if nPorts>0 then
     begin
         for i:=0 to nPorts-1 do
         begin
            lstPort.Items.Add(IntToStr(PortsList[i]));
         end;
         lstPort.ItemIndex:=0;
     end;

end;

procedure TfrmMain.cmdCaptureAndReadClick(Sender: TObject);
var
portNumber:Integer;
ComResponse:PChar;
aux:string;
Red:Byte;
Green:Byte;
Blue:Byte;
Intensity:Integer;
Hue:Single;
Saturation:Integer;
begin

     screen.Cursor := crHourGlass; //Change screen cursor to hourglass

     //Clear the Results box
     txtRed.Clear;

     //Set the port number
     portNumber := strtoint(lstPort.Items[lstPort.ItemIndex]);

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
     if FeasaCom_Open(portNumber,'57600')=1 then
     begin

          //Send command to the LED Analyser
          FeasaCom_Send(portNumber,'C',ComResponse);

          //Shows the received data in the screen
          txtRed.Text := txtRed.Text + ComResponse + ',';

          //read the number of the fiber to check and format it to 2 chars string number: 01, 05, 11, etc
          aux := RightStr('00' + txtFiber.Text,2);

          //DLL: Send command (Get RGBI data)
          if not FeasaCom_Send(portNumber,'GETRGBI' + aux,ComResponse)=1 then
          begin
               MessageDlg('Unable To Communicate with the LED Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(portNumber);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;

          //Parse response from the LED Analyser
          Feasa_Parse_RGBI(ComResponse, @Red, @Green, @Blue, @Intensity);

          //DLL: Send command (Get HSI data)
          if not FeasaCom_Send(portNumber,'GETHSI' + aux,ComResponse)=1 then
          begin
               MessageDlg('Unable To Communicate with the LED Analyser',mtWarning,[mbOK],0);
               FeasaCom_Close(portNumber);
               screen.Cursor := crDefault; //Change screen cursor to default
               Abort;
          end;

          //Parse response from the LED Analyser
          Feasa_Parse_HSI(ComResponse, @Hue, @Saturation, @Intensity);

          txtRed.Text := IntToStr(Red); //Show Red
          txtGreen.Text := IntToStr(Green); //Show Green
          txtBlue.Text := IntToStr(Blue); //Show Blue
          txtHue.Text := Format('%2.2f',[Hue]); //Show Hue
          txtSat.Text := IntToStr(Saturation); //Show Saturation
          txtInt.Text := IntToStr(Intensity); //Show Intensity

          //Close the port
          FeasaCom_Close(portNumber);
     end;

     screen.Cursor := crDefault; //Change screen cursor to default
end;

initialization
  {$I form1.lrs}

end.

