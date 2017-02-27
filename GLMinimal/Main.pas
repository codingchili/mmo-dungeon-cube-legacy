unit Main;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
{$IFDEF Win32}
  Windows, Vcl.ComCtrls, Vcl.Imaging.GIFImg, Vcl.Imaging.pngimage, JPeg,
{$ENDIF}
{$IFDEF Linux}
  cthreads, cmem, Types, {$IFDEF LCLGTK2} gtk2, gdk2, glib2, {$ENDIF}
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Math, AsphyreTypes;

type
  TRenderForm = class(TForm)
  private
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  private
    FailureHandled: Boolean;
    procedure OnAsphyreCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
    procedure OnAsphyreDestroy(Sender: TObject; Param: Pointer; var Handled: Boolean);
    procedure OnDeviceInit(Sender: TObject; Param: Pointer; var Handled: Boolean);
    procedure OnDeviceCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
    procedure OnTimerReset(Sender: TObject; Param: Pointer; var Handled: Boolean);

    procedure TimerEvent(Sender: TObject);
    procedure ProcessEvent(Sender: TObject);
    procedure RenderEvent(Sender: TObject);
    procedure HandleConnectFailure();
  end;

var
  RenderForm: TRenderForm;

const
  VSYNC = True;
  SPEED = 60.0;
  MAXFPS = 60;
  BUILD = 'Build 1.0.5';

implementation

{$R *.dfm}

uses

  System_Initializer, AsphyreEventTypes, AsphyreEvents, AsphyreTimer, AsphyreFactory, AsphyreArchives, AsphyreImages,
  AbstractDevices,
  AsphyreFonts, AbstractCanvas, NativeConnectors, Vectors2, Vectors2px,

{$IFDEF Win32}
  DX7Providers, WGLProviders, DX9Providers, DX10Providers, DX11Providers, AsphyrePNG,
{$ENDIF}
{$IFDEF Linux}
  XGLProviders,
{$ENDIF}
{$IFDEF MAC}
  AGLProviders,
{$ENDIF}
  GUI_Engine, System_UtilExt;

{$R *.dfm}

procedure TRenderForm.FormDestroy(Sender: TObject);
begin
  if (GameDevice <> nil) then
    GameDevice.Disconnect();
End;

procedure TRenderForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
end;

procedure TRenderForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
end;

procedure TRenderForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // create some kind of drag event
end;

procedure TRenderForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // create some kind of draw event
end;

procedure TRenderForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TRenderForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TRenderForm.FormResize(Sender: TObject);
begin
  Initializer.swidth := ClientWidth;
  Initializer.sheight := ClientHeight;

  if (GameDevice <> nil) then
  begin
    DisplaySize := Point2px(ClientWidth, ClientHeight);
    GameDevice.Resize(0, DisplaySize);
  end;
end;

procedure TRenderForm.HandleConnectFailure;
begin
  Timer.Enabled := False;
  Showmessage('Failed To Initialize.');
  close();
end;

procedure TRenderForm.OnAsphyreCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  GameDevice := Factory.CreateDevice();
  GameCanvas := Factory.CreateCanvas();
  GameImages := TAsphyreImages.create();

  GameFonts := TAsphyreFonts.create();
  GameFonts.images := GameImages;
  GameFonts.Canvas := GameCanvas;

  ArchiveTypeAccess := ataAnyFile;
end;

procedure TRenderForm.OnAsphyreDestroy(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  Timer.Enabled := False;
  GameFonts.free;
  GameImages.free;
  GameCanvas.free;
  FreeAndNil(GameDevice);
end;

procedure TRenderForm.OnDeviceCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  GameFonts.Insert('media/gui/charset.xml', 'charset.png');
  GameFonts[0].Kerning := 1.00;
  GameFonts[0].Scale := 1.00;
  PBoolean(Param)^ := True;
end;

procedure TRenderForm.OnTimerReset(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  Timer.Reset();
end;

procedure TRenderForm.ProcessEvent(Sender: TObject);
begin
  // call logics here
  writeln('processevent');
end;

procedure TRenderForm.RenderEvent(Sender: TObject);
begin
  // call draw on stuff here
  writeln('renderevent');
End;

procedure TRenderForm.TimerEvent(Sender: TObject);
begin
  if (not NativeAsphyreConnect.Init()) then
    exit();

  if (GameDevice <> nil) and (GameDevice.IsAtFault()) then
  begin
    if (not FailureHandled) then
      HandleConnectFailure();
    FailureHandled := True;
    exit;
  end;

  if (GameDevice = nil) or (not GameDevice.Connect()) then
    exit;

  Timer.Process();
  GameDevice.Render(RenderEvent, $FF000000);
end;

procedure TRenderForm.OnDeviceInit(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  DisplaySize := Point2px(ClientWidth, ClientHeight);
  GameDevice.SwapChains.Add(Self.Handle, DisplaySize);
  GameDevice.SwapChains.Items[0].VSYNC := VSYNC;
  GameCanvas.Antialias := True;
  GameCanvas.MipMapping := True;
end;

procedure TRenderForm.FormCreate(Sender: TObject);
begin
  System_Initializer.initialize;
  try
{$IFDEF Win32}
    Factory.UseProvider(idDirectX11);
    // dx7, dx9, dx10, dx11, openGL, idWinOpenGL
{$ENDIF}
{$IFDEF Linux}
    Factory.UseProvider(idLinuxOpenGL);
{$ENDIF}
{$IFDEF MAC}
    Factory.UseProvider(idAGL);
{$ENDIF}
    EventAsphyreCreate.Subscribe(ClassName, OnAsphyreCreate);
    EventAsphyreDestroy.Subscribe(ClassName, OnAsphyreDestroy);
    EventDeviceInit.Subscribe(ClassName, OnDeviceInit);
    EventDeviceCreate.Subscribe(ClassName, OnDeviceCreate);
    EventTimerReset.Subscribe(ClassName, OnTimerReset);
  except
    Showmessage('Failed to Initialize The Graphics Driver.');
  end;

  Timer.MAXFPS := MAXFPS;
  Timer.SPEED := SPEED;
  Timer.OnTimer := TimerEvent;
  Timer.OnProcess := ProcessEvent;
  Timer.Enabled := True;

  FailureHandled := False;
End;

END.

  end.
