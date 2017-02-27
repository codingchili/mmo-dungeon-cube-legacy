unit GUI_Patcher;

// todo: have different backgrounds for offline and online servers.

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

interface

uses GUI_Engine, GUI_Element, GUI_Element_Image, GUI_Element_Text, GUI_Element_Panel, GUI_Element_Button,
    System_UtilExt, GUI_Element_Background, Mouse_Proxy, Patching, Classes, SysUtils {$IFDEF Win32}, ShellApi,
    Windows{$ENDIF};

const
    RESISTANCE = 0.995;

type
    TFloatPoint = record
        x, y: single;
    end;

type
    TSplashMain = class
    private
        spinner, download: TGUIImage;
        launchbutton: TGUIButton;
        background: TGUIBackground;
        panel: TGUIPanel;
        status, rate: TGUIText;
        loadedbg, bgfading: boolean;
        lastmouse, thismouse: TPoint;
        deltaforce: TFloatPoint;
        textinit, buttoninit: boolean;
        spinnerspeed: single;
    public
        engine: TGUIEngine;
        procedure Logic;
        procedure Draw;
        procedure SetText(text: string; color: cardinal = 0);
        constructor create();
        procedure Launch();
    end;

procedure Initialize;

var
    SplashMain: TSplashMain;
    initialized: boolean;

implementation

uses Main, System_Initializer, Effects_Blending, Effects_Fire, Effects_Lightning, Settings_Patcher, Conf_Spritemap;

procedure Initialize();
begin
    SplashMain := TSplashMain.create;
    initialized := true;
end;

procedure TSplashMain.Launch();
begin
{$IFDEF Win32}
    ShellExecute(0, 'open', 'Game.exe', PChar(Patching.params), NIL, SW_SHOWNORMAL);
    ExitProcess(0);
{$ELSE}
    Halt;
{$ENDIF}
end;

// when loading, fade in logo,
// when logo faded in check if done
// when done, fade in background,
// when background in fading: hide spinner
// when background has faded, show login form.
// when form visible, fade in controls

procedure TSplashMain.SetText(text: string; color: cardinal = 0);
begin
    status.text := text;
    if (color <> 0) then
        spinner.filterwith(color);
end;

constructor TSplashMain.create;
begin
    engine := TGUIEngine.create('');
    loadedbg := false;
    bgfading := false;

    textinit := false;
    buttoninit := false;

    if (Settings.option.compact) then
        panel := TGUIPanel.create(pointer(engine), 1, 1, 298, 40)
    else
        panel := TGUIPanel.create(pointer(engine), 0, 1, Initializer.swidth, 42);

    panel.draggable := false;

    // System_UtilExt.Initialize;
    // todo: download these files first? (put at top of patch-conf, when any of these files are loaded rescan and loadall.
    // also image.update to set new posseh.)

    Patching.Initialize;
end;

// todo: in cfg: enable compact mode, 42x300, a movable with physics "always on top"
procedure TSplashMain.Logic();
begin
    if (Settings.option.autostart) and (Settings.option.compact) and (patcher.status.done) then
        download.rect.width := 0;

    if (GameImages.Resolve('download.png') = -1) then
    begin
        FileMan.FileSearch('media/patcher/', 'download.png');
        FileMan.Load();

        if (GameImages.Resolve('download.png') > -1) then
        begin
            download := TGUIImage.create(pointer(engine), 0, 0, 74, 42, 'download.png');
            download.setparent(pointer(panel));
            download.filterwith($FFFFFFFF);
            download.rect.width := 0;
            engine.pack;
        end;
    end
    else if (patcher.status.completed > 0) then
    begin
        patcher.lock;
        download.rect.width := round(panel.rect.width * (patcher.status.completed / patcher.status.filesizes));
        patcher.unlock;
    end;

    if (GameImages.Resolve('spinner.png') = -1) then
    begin
        FileMan.FileSearch('media/patcher/', 'spinner.png');
        FileMan.Load();

        if (GameImages.Resolve('spinner.png') > -1) then
        begin
            if (Settings.option.compact) then
                spinner := TGUIImage.create(pointer(engine), panel.rect.width - 24, 20, 34, 34, 'spinner.png')
            else
                spinner := TGUIImage.create(pointer(engine), Initializer.swidth - 24, 20, 34, 34, 'spinner.png');
            spinner.SetRotable(true);
            spinner.filterwith($FFFF0000);
            spinner.setparent(pointer(panel));
            spinnerspeed := -2;
            engine.pack;
        end;
    end
    else
    begin
        patcher.lock;

        if (patcher.status.disconnected = false) then
            spinner.filterwith($FF00FF00);

        spinnerspeed := 2 + (patcher.status.rate div 18500); // rate is bps
        if (spinnerspeed > 14) then
            spinnerspeed := 14;

        if (patcher.status.disconnected) then
            spinnerspeed := -2;

        if (patcher.status.done) then
        begin
            spinner.filterwith($FF00FF00);
            spinnerspeed := 4;
        end;

        if (patcher.status.disconnected = false) and (patcher.status.done = false) then
            spinner.filterwith($FFFFFF00);

        if (patcher.status.done) then
            spinner.Hide
        else
            spinner.Show;

        if (patcher.status.finished) then
        begin
            spinner.filterwith($FF00FF00);
            spinnerspeed := 4;
        end;

        if (patcher.status.disconnected) then
            spinner.filterwith($FFFF0000);

        spinner.rotation := spinner.rotation + round(spinnerspeed);

        patcher.unlock;
    end;

    if (GameImages.Resolve('launch.png') = -1) and (patcher.status.done) and not(buttoninit) and
      not(Settings.option.autostart) then
    begin
        FileMan.FileSearch('media/patcher/', 'launch.png');
        FileMan.Load();

        if (GameImages.Resolve('launch.png') <> -1) then
        begin

            launchbutton := TGUIButton.create(pointer(engine), 0, 0, Point(0, 0), Point(0, 0), '', self.Launch);

            launchbutton.spritesheet := GameImages.Resolve('launch.png');
            launchbutton.rect.top := 7;

            launchbutton.rect.width := 140;
            launchbutton.rect.height := 28;
            launchbutton.rect.left := Initializer.swidth div 2 - launchbutton.rect.width div 2;
            buttoninit := true;
        end;
    end;

    if (Settings.option.compact) and (textinit) then
        if (patcher.status.done) then
            status.Hide
        else
            status.Show;

    if buttoninit then
    begin
        if not(patcher.status.done) then
            launchbutton.Hide
        else
            launchbutton.Show;

        if launchbutton.hover then
            launchbutton.filterwith($FFFFFFFF)
        else
            launchbutton.filterwith($88FFFFFF);
    end;

    if (GameImages.Resolve('particle.png') < 0) then
    begin
        FileMan.FileSearch('media/patcher/', 'patcher_particle.png');
        FileMan.Load();
    end;

    if (GameImages.Resolve('charset.png') = -1) and FileExists('media/gui/charset.xml') then
    begin
        FileMan.FileSearch('media/gui/', 'charset.png');
        FileMan.Load();

        if (GameImages.Resolve('charset.png') > 0) then
        begin

            GameFonts.Insert('media/gui/charset.xml', 'charset.png');
            GameFonts[0].Kerning := 1.00;
            GameFonts[0].Scale := 1.00;

            status := TGUIText.create(pointer(engine), 8, 22, 'FFFF');
            status.setparent(pointer(panel));
            // status.color := $FFFF00CC;

            if (Settings.option.compact) then
                rate := TGUIText.create(pointer(engine), 8, 6, '')
            else
                rate := TGUIText.create(pointer(engine), Initializer.swidth div 2, 22, '');
            rate.setparent(pointer(panel));
            // rate.color := $FFFF00CC;
            textinit := true;

            engine.pack;
        end;
    end;

    if (textinit) then
    begin
        patcher.lock;

        if (patcher.status.disconnected) or (patcher.status.done) or (patcher.status.finished) then
            rate.Hide
        else
            rate.Show;

        status.text := patcher.status.text;
        if (patcher.status.rate > 0) then
            rate.text := RateFormat(patcher.status.rate)
        else
            rate.text := '';
        patcher.unlock;
    end;

    // do gravity, bounds checking etc.
    if (loadedbg) then
    begin
        if (GetAsyncKeyState(VK_LBUTTON) <> 0) then
        begin
            thismouse := TMouse.CursorPos;
            deltaforce.x := thismouse.x - lastmouse.x;
            deltaforce.y := thismouse.y - lastmouse.y;
        end;

        panel.rect.top := panel.rect.top + round(deltaforce.y);
        panel.rect.left := panel.rect.left + round(deltaforce.x);

        if (abs(deltaforce.y) > 0) then
            deltaforce.y := deltaforce.y * RESISTANCE;

        if (abs(deltaforce.x) > 0) then
            deltaforce.x := deltaforce.x * RESISTANCE;

        if (panel.rect.top + deltaforce.y < 0) or
          (panel.rect.top + panel.rect.height + deltaforce.y > Initializer.sheight) then
            deltaforce.y := deltaforce.y * -1;
        if (panel.rect.left + deltaforce.x < 0) or
          (panel.rect.left + panel.rect.width + deltaforce.x > Initializer.swidth) then
            deltaforce.x := deltaforce.x * -1;

        lastmouse := TMouse.CursorPos;
    end;

    { if (random(2) = 0) then
      FireMan.Add(spinner.rect.left, 42, 300);

      if (random(8) = 0) then
      LightningMan.Add(Point(spinner.rect.left, 20), Point(spinner.rect.left - 175, 20), 30);

      if (random(8) = 0) then
      LightningMan.Add(Point(spinner.rect.left, 20), Point(spinner.rect.left + 175, 20), 30); }

    { if not(loadedbg) and (GameImages.Resolve('background.jpg') > 0) then
      begin
      background := TGUIBackground.create(pointer(engine), 'background.jpg');
      engine.pack;
      loadedbg := true;
      background.fadein;
      end; }

    engine.Logic;
end;

procedure TSplashMain.Draw();
begin
    // status.text := 'in draw';
    engine.Draw;
end;

end.
