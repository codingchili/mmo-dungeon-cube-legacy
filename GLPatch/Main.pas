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
    Dialogs, ExtCtrls, StdCtrls, Math, AsphyreTypes, Vcl.Menus, System_UtilExt;

type
    TRenderForm = class(TForm)
        popupmenu: TPopupMenu;
        ShowVersion1: TMenuItem;
        Speed1: TMenuItem;
        Filename1: TMenuItem;
        FilesLeft1: TMenuItem;
        Downloaded1: TMenuItem;
        ServerIp1: TMenuItem;
        ServerGeo1: TMenuItem;
        Settings1: TMenuItem;
        Close1: TMenuItem;
        SetRegion1: TMenuItem;
        EU1: TMenuItem;
        NA1: TMenuItem;
        AS1: TMenuItem;
        Custom1: TMenuItem;
        Autostart1: TMenuItem;
        ResetVersion1: TMenuItem;
        smooth: TTimer;
        Compact1: TMenuItem;
        Label1: TLabel;
        procedure FormDestroy(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure CreateParams(var Params: TCreateParams); override;
        procedure Close1Click(Sender: TObject);
        procedure Autostart1Click(Sender: TObject);
        procedure ResetVersion1Click(Sender: TObject);
        procedure Compact1Click(Sender: TObject);
        procedure Custom1Click(Sender: TObject);
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

const
    VSYNC = False;
    SPEED = 60.0;
    MAXFPS = 60;
    BUILD = '1.0.8';

var
    RenderForm: TRenderForm;

implementation

{$R *.dfm}

uses

    System_Initializer, AsphyreEventTypes, AsphyreEvents, AsphyreTimer, AsphyreFactory, AsphyreArchives, AsphyreImages,
    AbstractDevices, Effects_Fire, Effects_Lightning, Mouse_Proxy, Settings_Patcher,
    AsphyreFonts, AbstractCanvas, NativeConnectors, Vectors2, Vectors2px, GUI_Patcher, Engine_Particles, Patching,
    GUI_Element, Effects_Smooth,

{$IFDEF Win32}
    DX7Providers, WGLProviders, DX9Providers, DX10Providers, DX11Providers, AsphyrePNG,
{$ENDIF}
{$IFDEF Linux}
    XGLProviders,
{$ENDIF}
{$IFDEF MAC}
    AGLProviders,
{$ENDIF}
    GUI_Engine, System_Audio;

procedure TRenderForm.FormDestroy(Sender: TObject);
begin
    if (GameDevice <> nil) then
        GameDevice.Disconnect();
End;

procedure TRenderForm.Autostart1Click(Sender: TObject);
begin
    if (TMenuItem(Sender).Checked = False) then
        TMenuItem(Sender).Checked := True
    else
        TMenuItem(Sender).Checked := False;

    Settings.option.autostart := TMenuItem(Sender).Checked;

    if (Settings.option.autostart) then
        showmessage('To disable Autostart edit conf/patcher.cfg');

    Settings.save;
end;

procedure TRenderForm.Close1Click(Sender: TObject);
begin
    application.Terminate;
end;

procedure TRenderForm.Compact1Click(Sender: TObject);
begin
    if (TMenuItem(Sender).Checked = False) then
        TMenuItem(Sender).Checked := True
    else
        TMenuItem(Sender).Checked := False;

    Settings.option.compact := TMenuItem(Sender).Checked;
    Settings.save;
end;

procedure TRenderForm.CreateParams(var Params: TCreateParams);
begin
    inherited;
    Params.ExStyle := Params.ExStyle and not WS_EX_APPWINDOW;
    Params.WndParent := application.Handle;
end;

procedure TRenderForm.Custom1Click(Sender: TObject);
var
    input: string;
    list: TStringList;
    error: Boolean;
    versionfile: TextFile;
begin
    error := False;
    input := InputBox('Patch Server [IP:PORT]', 'IP', Settings.option.host + ':' + inttostr(Settings.option.port));

    list := TStringList.Create;
    try
        System_UtilExt.Split(':', input, list);

        patcher.lock;
        Settings.option.host := list.Strings[0];
        Settings.option.port := StrToInt(list.Strings[1]);
        Settings.save;
        patcher.unlock;
    except
        showmessage('Invalid Entry Format, Please Use: IP:PORT.');
        error := True;
    end;
    list.Free;

    if not error then
    begin
        patcher.Free;
        patcher.Yield;
        patcher := TPatcher.Create;
    end;
end;

procedure TRenderForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

    // create some kind of drag event
    if (Button = TMouseButton.mbRight) then
        popupmenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    SplashMain.engine.MouseEvent(TGUIMouseEvent.left_down);
end;

procedure TRenderForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    // create some kind of draw event
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
    showmessage('Failed To Initialize.');
    close();
end;

procedure TRenderForm.OnAsphyreCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    GameDevice := Factory.CreateDevice();
    GameCanvas := Factory.CreateCanvas();
    GameImages := TAsphyreImages.Create();

    GameFonts := TAsphyreFonts.Create();
    GameFonts.images := GameImages;
    GameFonts.Canvas := GameCanvas;

    ArchiveTypeAccess := ataAnyFile;
end;

procedure TRenderForm.OnAsphyreDestroy(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    Timer.Enabled := False;
    GameFonts.Free;
    GameImages.Free;
    GameCanvas.Free;
    FreeAndNil(GameDevice);
end;

procedure TRenderForm.OnDeviceCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    GUI_Patcher.initialize;
    PBoolean(Param)^ := True;
end;

procedure TRenderForm.OnTimerReset(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    Timer.Reset();
end;

procedure TRenderForm.ProcessEvent(Sender: TObject);
begin
    SplashMain.Logic;
    { ParticleMan.orphan;
      ParticleMan.movement;
      FireMan.process;
      Lightningman.process;
      ParticleMan.flushbuffer; }
end;

procedure TRenderForm.RenderEvent(Sender: TObject);
begin
    SplashMain.Draw;
   // ParticleMan.Draw;
   // LightningMan.Draw;
End;

procedure TRenderForm.ResetVersion1Click(Sender: TObject);
begin
    if fileexists('conf/version.dat') then
        DeleteFile('conf/version.dat');

    patcher.Free;
    patcher.Yield;
    patcher := TPatcher.Create;
end;

procedure TRenderForm.TimerEvent(Sender: TObject);
begin
    // application.CancelHint;
    // application.DoApplicationIdle;
    // application.ProcessMessages;
    // application.Run;

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

    if GameImages.Resolve('charset.png') = -1 then
    begin
        RenderForm.Label1.Repaint;
        RenderForm.Label1.Invalidate;
        patcher.lock;
        RenderForm.Label1.Caption := patcher.status.text;
        patcher.unlock;
    end
    else
    begin
        Label1.Visible := False;
        Label1.Invalidate;
        GameDevice.Render(RenderEvent, $FF000000);
    end;
end;

procedure TRenderForm.OnDeviceInit(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    DisplaySize := Point2px(ClientWidth, ClientHeight);
    GameDevice.SwapChains.Add(self.Handle, DisplaySize);
    GameDevice.SwapChains.Items[0].VSYNC := VSYNC;
    GameCanvas.Antialias := True;
    GameCanvas.MipMapping := True;
end;

procedure TRenderForm.FormCreate(Sender: TObject);
begin
    System_Initializer.initialize;
    System_UtilExt.initialize;
    Settings_Patcher.initialize;
    Effects_Smooth.initialize;

    Autostart1.Checked := Settings.option.autostart;
    Compact1.Checked := Settings.option.compact;

    if (Settings.option.compact) then
    begin
        RenderForm.Align := TAlign.alNone;
        RenderForm.width := 300;
        RenderForm.height := 42;
        RenderForm.top := screen.height - 96;
        RenderForm.left := screen.width - 325;
    end;

    try
{$IFDEF Win32}
        Factory.UseProvider(idDirectX11);
        System_Audio.initialize;
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
        showmessage('Failed to Initialize The Graphics Driver.');
    end;

    Timer.MAXFPS := MAXFPS;
    Timer.SPEED := SPEED;
    Timer.OnTimer := TimerEvent;
    Timer.OnProcess := ProcessEvent;
    Timer.Enabled := True;

    FailureHandled := False;
End;

END.
