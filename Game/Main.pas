unit Main;

// this unit contains drawing.
// instead of using screen.heigh/width use SWidth/SHeight!!!

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
{$IFDEF Win32}
    Windows, Vcl.ComCtrls, Vcl.Imaging.GIFImg, Vcl.Imaging.pngimage, JPeg,
{$ENDIF}
{$IFDEF Linux}
    cthreads, cmem, Types, {$IFDEF LCLGTK2} gtk2, gdk2, glib2, {$ENDIF}
{$ENDIF}
    Messages, Debug_Stopwatch, SysUtils, Variants, Classes, Graphics, Controls,
    Forms,
    Dialogs, ExtCtrls, StdCtrls, Math, AsphyreTypes;

type
    TRenderForm = class(TForm)
        procedure openview();
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
    public
        { Public declarations }
    end;

type
    TPerformanceStats = record
        proc_avg, render_avg, proc_update, render_update: Integer;
        procticks, renderticks: single;
        msproc, msrender: string[10];
    end;

type
    TView = (TCharacterSelect, TServerSelect, TGameInterface, TLogin, TSplash, TNone);

const
    VSYNC = True;
    SPEED = 120.0;
    MAXFPS = 120;
    BUILD = 'Build 1.0.8';
    render_wait: Integer = 30;
    proc_wait: Integer = 30;

procedure Terminate;
procedure ProcessMessages;

var
    sw: TStopwatch;
    RenderForm: TRenderForm;
    View: TView;
    PerformanceStats: TPerformanceStats;
    initgui: Boolean;
    colors: cardinal;

implementation

uses
    System_Initializer, System_Camera, System_Keyboard, System_Log,

    AsphyreEventTypes, AsphyreEvents, AsphyreTimer, AsphyreFactory,
    AsphyreArchives, AsphyreImages, AbstractDevices, AsphyreFonts, AbstractCanvas,
    NativeConnectors, Vectors2, Vectors2px,

{$IFDEF Win32}
    DX7Providers, WGLProviders, DX9Providers, DX10Providers,
    DX11Providers, System_Audio, AsphyrePNG,
{$ENDIF}
{$IFDEF Linux}
    XGLProviders,
{$ENDIF}
{$IFDEF MAC}
    AGLProviders,
{$ENDIF}
    GUI_Chat, GUI_Bars, GUI_Menu, GUI_StatusBox, GUI_Element, GUI_License,
    GUI_Banner, GUI_SkillBar, GUI_Inventory,
    Conf_Spritemap, Conf_Protocol,

    GUI_Engine, Engine_Particles, Engine_Map, Engine_Projectiles, Engine_Player,
    GUI_CharSelect, GUI_ServerSelect, GUI_LoginScreen, GUI_Targeting,

    System_Ping, System_Multiplayer, System_Text, System_Spells, System_DayTime,
    System_UtilExt, System_GUIStrapper,

    Network_WorldServer, Network_World,

    Effects_Lightning, Effects_Fire, Effects_Blending, Effects_Splat, Effects_Smoke, Effects_Water

    {ifdef DEBUG} , Automated_Tests {endif} , GUI_SplashMain, GUI_MiniMap;

{$R *.dfm}

procedure Terminate;
begin
    application.Terminate;
end;

procedure ProcessMessages;
begin
    application.ProcessMessages;
end;

procedure TRenderForm.FormDestroy(Sender: TObject);
begin
    if (GameDevice <> nil) then
        GameDevice.Disconnect();
End;

procedure TRenderForm.openview();
begin
    View := TView.TNone;
end;

procedure TRenderForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    KeyEvent: TGUIKeyEvent;
begin
    if not initgui then
        exit;

    Keyboard.chars[Key] := False;

    if (View = TView.TGameInterface) then
        Keyboard.kbUp(Key);

    KeyEvent.Key := Key;
    KeyEvent.state := TGUIKeyState.up;

    if (statusbox.frame.visible) then
    begin
        statusbox.engine.KeyEvent(TGUIKeyEvent(KeyEvent));
        exit;
    end;

    if licensebox.frame.visible then
    begin
        licensebox.engine.KeyEvent(TGUIKeyEvent(KeyEvent));
        exit();
    end;

    // forward to guiEngineView
    case (View) of
        TView.TCharacterSelect:
            CharSelect.engine.KeyEvent(KeyEvent);
        TView.TServerSelect:
            ServerScreen.engine.KeyEvent(KeyEvent);
        TView.TGameInterface:
            begin
                ChatEngine.engine.KeyEvent(KeyEvent);
                GUIBar.engine.KeyEvent(KeyEvent);
                GUITargetFrame.engine.KeyEvent(KeyEvent);
                MenuGUI.engine.KeyEvent(KeyEvent);
                GUIInventory.engine.KeyEvent(KeyEvent);
                SkillBar.engine.KeyEvent(KeyEvent);
                GUIMiniMap.engine.KeyEvent(KeyEvent);
            end;
        TView.TLogin:
            LoginScreen.engine.KeyEvent(KeyEvent);
    end;
end;

procedure TRenderForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    KeyEvent: TGUIKeyEvent;
begin
    if not initgui then
        exit;

    Keyboard.input := Keyboard.input + Keyboard.GetCharFromVKey(Key);

    if not(Keyboard.inputlock) then
        Keyboard.chars[Key] := True;

    if (View = TView.TGameInterface) then
        Keyboard.kbDown(Key);

    KeyEvent.Key := Key;
    KeyEvent.state := TGUIKeyState.down;

    if (statusbox.frame.visible) then
    begin
        statusbox.engine.KeyEvent(TGUIKeyEvent(KeyEvent));
        exit;
    end;

    if licensebox.frame.visible then
    begin
        licensebox.engine.KeyEvent(TGUIKeyEvent(KeyEvent));
        exit();
    end;

    // forward to guiEngineView
    case (View) of
        TView.TCharacterSelect:
            CharSelect.engine.KeyEvent(KeyEvent);
        TView.TServerSelect:
            ServerScreen.engine.KeyEvent(KeyEvent);
        TView.TGameInterface:
            begin
                ChatEngine.engine.KeyEvent(KeyEvent);
                GUIBar.engine.KeyEvent(KeyEvent);
                GUITargetFrame.engine.KeyEvent(KeyEvent);
                MenuGUI.engine.KeyEvent(KeyEvent);
                GUIInventory.engine.KeyEvent(KeyEvent);
                SkillBar.engine.KeyEvent(KeyEvent);
                GUIMiniMap.engine.KeyEvent(KeyEvent);
            end;
        TView.TLogin:
            LoginScreen.engine.KeyEvent(KeyEvent);
    end;
end;

procedure TRenderForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    mode: TGUIMouseEvent;
begin
    if not initgui then
        exit;

    if (Button = TMouseButton.mbLeft) then
        Keyboard.chars[1] := True
    else
        Keyboard.chars[0] := True;

    if (Button = TMouseButton.mbLeft) then
        mode := TGUIMouseEvent.left_down
    else
        mode := TGUIMouseEvent.right_down;

    if licensebox.frame.visible then
    begin
        licensebox.engine.MouseEvent(mode);
        exit();
    end;

    if statusbox.frame.visible then
    begin
        statusbox.engine.MouseEvent(mode);
        exit();
    end;

    // forward to guiEngineView
    case (View) of
        TView.TCharacterSelect:
            CharSelect.engine.MouseEvent(mode);
        TView.TServerSelect:
            ServerScreen.engine.MouseEvent(mode);
        TView.TGameInterface:
            begin
                MenuGUI.engine.MouseEvent(mode);
                GUIBar.engine.MouseEvent(mode);
                GUITargetFrame.engine.MouseEvent(mode);
                ChatEngine.engine.MouseEvent(mode);
                SkillBar.engine.MouseEvent(mode);
                GUIInventory.engine.MouseEvent(mode);
                GUIMiniMap.engine.MouseEvent(mode);
            end;
        TView.TLogin:
            LoginScreen.engine.MouseEvent(mode);
    end;

end;

procedure TRenderForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    mode: TGUIMouseEvent;
begin
    if not initgui then
        exit;

    if (Button = TMouseButton.mbLeft) then
        Keyboard.chars[1] := False
    else
        Keyboard.chars[0] := False;

    if (Button = TMouseButton.mbLeft) then
        mode := TGUIMouseEvent.left_up
    else
        mode := TGUIMouseEvent.right_up;

    if licensebox.frame.visible then
    begin
        licensebox.engine.MouseEvent(mode);
        exit();
    end;

    if statusbox.frame.visible then
    begin
        statusbox.engine.MouseEvent(mode);
        exit();
    end;

    // forward to guiEngineView
    case (View) of
        TView.TCharacterSelect:
            CharSelect.engine.MouseEvent(mode);
        TView.TServerSelect:
            ServerScreen.engine.MouseEvent(mode);
        TView.TGameInterface:
            begin
                ChatEngine.engine.MouseEvent(mode);
                GUIBar.engine.MouseEvent(mode);
                GUITargetFrame.engine.MouseEvent(mode);
                MenuGUI.engine.MouseEvent(mode);
                GUIInventory.engine.MouseEvent(mode);
                SkillBar.engine.MouseEvent(mode);
                GUIMiniMap.engine.MouseEvent(mode);
            end;
        TView.TLogin:
            LoginScreen.engine.MouseEvent(mode);
    end;
end;

procedure TRenderForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    case (View) of
        TView.TGameInterface:
            ChatEngine.NotifyScroll(TDirection.down);
    end;

    Handled := False;
end;

procedure TRenderForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
    case (View) of
        TView.TGameInterface:
            ChatEngine.NotifyScroll(TDirection.up);
    end;

    Handled := False;
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

    if (initgui) then
    begin
        LoginScreen.Resized;
        statusbox.Resized;
        GUIBanner.Resized;
        ServerScreen.Resized;
        CharSelect.Resized;
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
    // files that needs to be loaded before loader.
    System_UtilExt.initialize;
    FileMan.FileSearch('media/gui/', 'spinner.png');
    FileMan.FileSearch('media/gui/', 'charset.png');
    FileMan.Load();

    GameFonts.Insert('media/gui/charset.xml', 'charset.png');
    GameFonts[0].Kerning := 1.00;
    GameFonts[0].Scale := 1.00;

    // load spinner from controls
    // GUI_Windowed.initialize;
    GUI_SplashMain.initialize;
    System_GUIStrapper.initialize;
    GUI_StatusBox.initialize;
    GUI_MiniMap.initialize;
    Main.View := TView.TSplash;

    PBoolean(Param)^ := True;
end;

procedure TRenderForm.OnTimerReset(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    Timer.Reset();
end;

procedure TRenderForm.ProcessEvent(Sender: TObject);
var
    i: Integer;
begin
    sw.Start;

    { ifdef DEBUG }
    Automated_Tests.Run();
    { endif }

    Keyboard.Process; // fetch key events

    ParticleMan.orphan;
    ParticleMan.movement;
    ParticleMan.flushbuffer;

    case View of
        TView.TSplash:
            begin
                SplashMain.Logic;
            end;
        TView.TLogin:
            begin
                waterman.Process;
                licensebox.Logic;
                LoginScreen.Logic;
                statusbox.Logic;
                GUIBanner.Logic;
            end;
        TView.TServerSelect:
            begin
                statusbox.Logic;
                ServerScreen.Logic;
                GUIBanner.Logic;
            end;
        TView.TCharacterSelect:
            begin
                statusbox.Logic;
                CharSelect.Logic;
                GUIBanner.Logic;
            end;
        TView.TGameInterface:
            begin
                cam.Update;
                statusbox.Logic;
                MenuGUI.Logic;
                GUIBar.Logic;
                GUITargetFrame.Logic;
                ChatEngine.Logic;
                SkillBar.Logic;
                GUIInventory.Logic;

                Player.movement;
                Multiplayer.movement;

                ProjectileMan.orphan;
                ProjectileMan.movement;
                ProjectileMan.flushbuffer;

                ChatEngine.Process;
                TextEngine.Process;

                Player.sprite.Update;

                for i := 0 to length(Multiplayer.Player) - 1 do
                    Multiplayer.Player[i].sprite.Update;

                SpellMan.Process;
                FireMan.Process;
                SmokeMan.Process;
                LightningMan.Process;
                SplatMan.Process;
                DayTime.Process;
                GUIBanner.Logic;
                GUIMiniMap.Logic;
            end;
    end;

    Keyboard.input := '';
    sw.Stop;

    with PerformanceStats do
    begin
        proc_avg := sw.ElapsedTicks + proc_avg;
        dec(proc_update);

        if proc_update < 0 then
        begin
            procticks := proc_avg / proc_wait;
            // msproc := FormatFloat('0.###', procticks / 10000);
            msproc := inttostr(trunc(procticks));
            proc_avg := 0;
            proc_update := proc_wait;
        end;
    end;

end;

procedure TRenderForm.RenderEvent(Sender: TObject);
var
    i: Integer;
begin
    sw.Start;

    case View of
        TView.TSplash:
            SplashMain.Draw;
        TView.TLogin:
            begin
                // waterman.Draw;
                ParticleMan.Draw;
                LoginScreen.Draw;
                statusbox.Draw;
                licensebox.Draw;
                GUIBanner.Draw;
            end;
        TView.TServerSelect:
            begin
                ServerScreen.Draw;
                statusbox.Draw;
                GUIBanner.Draw;
            end;
        TView.TCharacterSelect:
            begin
                CharSelect.Draw;
                statusbox.Draw;
                GUIBanner.Draw;
            end;
        TView.TGameInterface:
            begin

                MapMan.DrawMap;
                // ProjectileMan.Draw;
                MapMan.DrawEntity(False);
                // GameCanvas.UseImagePt(MapMan.Image, 0);
                // GameCanvas.TexMap(pBounds4( { 254.0 + } cam.X,
                // { 254.0 + } cam.Y, MapMan.width, MapMan.height), blend.color,
                // TBlendingEffect.beNormal);

                if (keyboard.getasynckeystate(VK_SHIFT) <> 0) then
                begin

                    GameCanvas.UseImagePx(GameImages.Image[Player.sprite.sheet], Player.sprite.position);
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), Player.sprite.size.X, Player.sprite.size.Y),
                      cColor4($FFFFFFFF), TBlendingEffect.beNormal);
                end
                else
                begin
                    { render new character models here }

                    if (keyboard.getasynckeystate(VK_UP) = System_Keyboard.KEYBOARD_PRESS) then
                        case (colors) of
                            $FFCC0000:
                                colors := $FF00CC00;

                            $FF00CC00:
                                colors := $FF0000CC;

                            $FF0000CC:
                                colors := $FFCC0000;
                        end;
                    { result := Point4(origin.x + (current * size.x), origin.y, origin.x + size.x * (current + 1), origin.y,
                      origin.x + size.x * (current + 1), origin.y + size.y, origin.x + current * size.x, origin.y + size.y); }

                    { GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 1, 69, 1, 69, 94, 1, 94));

                      GameCanvas.TexMap(pRotate4(point2(Player.X + cam.X + 28, Player.Y + cam.Y + 10), point2(59, 94), point2(34, 47),
                      DegToRad(45), 1), cColor4($66000000), TBlendingEffect.beNormal); }

                    // body
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 1, 69, 1, 69, 94, 1, 94));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4($FFFFFFFF),
                      TBlendingEffect.beNormal);

                    // top
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 191, 69, 191, 69, 284, 1, 284));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4(colors),
                      TBlendingEffect.beNormal);

                    // bottom
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 286, 69, 286, 69, 379, 1, 379));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4(colors),
                      TBlendingEffect.beNormal);

                    // face
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 476, 69, 476, 69, 569, 1, 569));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4($FFFFFFFF),
                      TBlendingEffect.beNormal);

                    // eyes
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 381, 69, 381, 69, 474, 1, 474));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4(colors),
                      TBlendingEffect.beNormal);

                    // hair
                    GameCanvas.UseImagePx(GameImages.Image['chartest.png'], Point4(1, 96, 69, 96, 69, 189, 1, 189));
                    GameCanvas.TexMap(pBounds4(trunc(Player.X + cam.X - Player.sprite.size.X / 2),
                      trunc(Player.Y + cam.Y - Player.sprite.size.Y / 2), 69, 93), cColor4(colors),
                      TBlendingEffect.beNormal);

                    { END OF NEW CHARACTER MODELS }
                end;

                // drawline test:
               { for i := 1 to 50 do
                begin

                    ParticleMan.Fire(Player.X + 5 * i * sin(Math.ArcTan2(Multiplayer.Player[0].X - Player.X,
                      Multiplayer.Player[0].Y - Player.Y)),
                      Player.Y + 5 * i * cos(Math.ArcTan2(Multiplayer.Player[0].X - Player.X,
                      Multiplayer.Player[0].Y - Player.Y)), 3, DegToRad(random(360)), 20, TParticleEffect.peFumeIfy,
                      $FFFF6464, 12); // FF00CC64, FF66CC64  FFCC3299 FFFF6464
                end;     }

                 {GameCanvas.FrameRect(Rect(round(Player.X + cam.X + 22), round(Player.Y + cam.Y + 6),
                  round(Player.X + cam.X + 22 + 24), round(Player.Y + cam.Y + 64)), cColor4($FF00FF00), beAdd);    }

                SplatMan.Draw;

                for i := 0 to length(Multiplayer.Player) - 1 do
                begin
                    with (Multiplayer.Player[i]) do
                    begin
                        GameCanvas.UseImagePx(GameImages.Image[sprite.sheet], sprite.position);
                        GameCanvas.TexMap(pBounds4(round(Multiplayer.Player[i].X + cam.X - sprite.size.X / 2),
                          round(Multiplayer.Player[i].Y + cam.Y - sprite.size.Y / 2), sprite.size.X, sprite.size.Y),
                          cColor4($FFFFFFFF), TBlendingEffect.beNormal);
                    end;
                end;


                // projectiles are drawn before trees, all required is collision with base, depth buffering not required!

                MapMan.DrawEntity(True);
                // DayTime.Draw;

                ProjectileMan.Draw;

                ParticleMan.Draw;
                LightningMan.Draw;

                GUIMiniMap.Draw();
                ChatEngine.Render;
                TextEngine.Render;
                GUIInventory.Draw;
                SkillBar.Draw;
                ChatEngine.Draw;
                GUITargetFrame.Draw;
                GUIBar.Draw;
                MenuGUI.Draw;
                statusbox.Draw;
                GUIBanner.Draw;
            end;
    end;

    sw.Stop;

    if (Initializer.Developer) then
    begin
        GameCanvas.FillRect({$IFDEF Linux}Types.{$ENDIF}Rect(Initializer.swidth - 125, 2, Initializer.swidth - 2,
          295 + 40), $AA000000);

        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 28), 'FrameTime: ' + inttostr(trunc(Timer.Latency)),
          cColor2($FF00FFFF));

        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 8), 'FPS:   ' + inttostr(Timer.FrameRate),
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 28), 'FrameTime: ' + inttostr(trunc(Timer.Latency)),
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 48), 'Stall: ' + inttostr(GameCanvas.CacheStall),
          cColor2($FFFF0000));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 68), 'Entities: ' + inttostr(ProjectileMan.prjCount()),
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 88), 'Particls: ' + inttostr(ParticleMan.prtCount()),
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 108), 'Render: ' + PerformanceStats.msrender,
          cColor2($FFFF0000));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 128), 'Proccs: ' + PerformanceStats.msproc,
          cColor2($FFFF0000));
        if (System_Ping.enabledping) then
            GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 148), 'ICMP: ' + inttostr(System_Ping.RoundTripMs),
              cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 168), 'T-PING: ' + Network_WorldServer.TPing.PingTimeStr,
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 188), 'U-PING: ' + Network_World.UPing.PingTimeStr,
          cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 208), 'X: ' + inttostr(trunc(cam.X)) + ' Y:' +
          inttostr(trunc(cam.Y)), cColor2($FF00FFFF, $FF00FFFF));

        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 228), 'Time: ' + DayTime.FormatTime,
          cColor2($FF00FFFF, $FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 248),

          'RTime: ' + DayTime.RealTime, cColor2($FF00FFFF, $FF00FFFF));

        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 268), '>> ' + Protocol.NETIOO, cColor2($FF00FFFF));
        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 288), '<< ' + Protocol.NetIOI, cColor2($FF00FFFF));

        GameFonts.Items[0].TextOut(Point2(DisplaySize.X - 105, 308), BUILD, cColor2($FF00FFFF));

        with PerformanceStats do
        begin
            render_avg := sw.ElapsedTicks + render_avg;
            dec(render_update);

            if render_update < 0 then
            begin
                renderticks := render_avg / render_wait;
                // msrender := FormatFloat('0.###', renderticks / 10000);
                msrender := inttostr(trunc(renderticks));
                render_avg := 0;
                render_update := render_wait;
            end;
        end;
    end;

    if (Main.View <> TView.TGameInterface) then
    begin

        GameFonts.Items[0].TextOut(Point2(8, DisplaySize.Y - 16), 'Developer Build ' + DateToStr(now),
          cColor2($FF00FFFF, $FF00FFFF));

        GameFonts.Items[0].TextOut(Point2(8, DisplaySize.Y - 30), 'Version ' + Initializer.version,
          cColor2($FF00FFFF, $FF00FFFF));
    end;

End;

procedure TRenderForm.TimerEvent(Sender: TObject);
begin
    try
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

    if (Main.View <> TView.TNone) then
    begin
        Timer.Process();
        GameDevice.Render(RenderEvent, $FF000000 { FF66A7F6 } );
    end;
    except
        showmessage('Error, Missing video support.'+ #13+#10 + {$IFDEF Win32}'- DirectX 11.0'{$ELSE} {$IFDEF Linux}'- OpenGL 2.0'{$ELSE} '- Unknown. [Mac?]'{$ENDIF}{$ENDIF}  );
        halt;
    end;
end;

procedure TRenderForm.OnDeviceInit(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
    DisplaySize := Point2px(ClientWidth, ClientHeight);
    GameDevice.SwapChains.Add(Self.Handle, DisplaySize);
    GameDevice.SwapChains.Items[0].VSYNC := VSYNC;
    GameCanvas.Antialias := True;
    GameCanvas.MipMapping := True;
end;

{ class procedure TRenderForm.WideException(Sender: TObject);
  begin
  // exception. probably.. log these.
  Showmessage('Some Error Occured.');
  application.Terminate;
  end;

  procedure TRenderForm.ApplicationEvents1Exception(Sender: TObject;
  E: Exception);
  var
  logfile: TextFile;
  begin
  // Showmessage('Error... ' + E.Message);
  raise E;

  try
  assignfile(logfile, 'conf/logfile.txt');

  if (fileexists('conf/logfile.txt')) then
  append(logfile)
  else
  rewrite(logfile);

  writeln(logfile, E.Message);
  closefile(logfile);
  finally
  end;

  if (Main.View <> TView.Tnone) then
  try
  statusbox.show;
  statusbox.settext('Error: ' + E.Message);
  sleep(3000);
  {$IFDEF Win32 }
// ExitProcess(0);
// {$ELSE}
// Halt;
// {$ENDIF}
// finally
// might not have been initialized yet.
// end;
// end;   }

procedure TRenderForm.FormCreate(Sender: TObject);
begin
    System_Initializer.initialize;

    // WindowState := wsNormal;  - werks
    colors := $FFCC0000;

    if not(Initializer.windowed) then
    begin
        Self.BorderStyle := bsNone;
        Self.WindowState := wsMaximized;
        width := screen.width;
        height := screen.height;
{$IFDEF Linux}
{$IFDEF LCLGTK2}
       // gdk_window_fullscreen(PGtkWidget(Handle)^.window);
{$ENDIF}
{$ELSE}
        ShowWindow(Handle, SW_SHOWMAXIMIZED);
{$ENDIF}
    end
    else
    begin
        WindowState := wsNormal;
        Top := (screen.height - Self.height) div 2;
        Left := (screen.width - Self.width) div 2;
    end;

    View := TView.TNone;
    initgui := False;

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

    // loadbar.left := 50;
    // loadbar.width := screen.width - 100;
    // loadbar.top := screen.height - loadbar.height - 30;

    Self.BringToFront;

    sw := TStopwatch.create(False);

    Timer.MAXFPS := MAXFPS;
    Timer.SPEED := SPEED;
    Timer.OnTimer := TimerEvent;
    Timer.OnProcess := ProcessEvent;
    Timer.Enabled := True;
    Self.TimerEvent(nil);

    // animated spinner
    { Image1.Picture.LoadFromFile('media/gui/spinner.gif');
      (Image1.Picture.Graphic as TGIFImage).AnimateLoop := glEnabled;
      (Image1.Picture.Graphic as TGIFImage).Animate := True; }
    // Timer.Enabled := True;
    FailureHandled := False;
End;

END.