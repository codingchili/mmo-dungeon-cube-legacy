unit GUI_SplashMain;

// todo: have different backgrounds for offline and online servers.

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

interface

uses GUI_Engine, GUI_Element, GUI_Element_Image, GUI_Element_Text,
  GUI_Element_Panel, Classes,
  System_UtilExt, GUI_Element_Background, Mouse_Proxy, SysUtils, System_Keyboard
{$IFDEF Win32}, Windows{$ENDIF};

const
  GRAVITY = 0.97;
  RESISTANCE = 0.975;

type
  TFloatPoint = record
    x, y: single;
  end;

type
  TSplashMain = class
  private
    spinner, download: TGUIImage;
    background: TGUIBackground;
    panel: TGUIPanel;
    status: TGUIText;
    progress: integer;
    loadedbg, bgfading: boolean;
    lastmouse, thismouse: TPoint;
    deltaforce: TFloatPoint;
  public
    engine: TGUIEngine;
    procedure Logic;
    procedure Draw;
    procedure SetText(text: string; color: cardinal = 0; progress: integer = 0);
    constructor create();
  end;

procedure Initialize;

var
  SplashMain: TSplashMain;
  initialized: boolean;

implementation

uses Main, System_Initializer, Effects_Blending;

procedure Initialize();
begin
  SplashMain := TSplashMain.create;
  initialized := true;
end;

// when loading, fade in logo,
// when logo faded in check if done
// when done, fade in background,
// when background in fading: hide spinner
// when background has faded, show login form.
// when form visible, fade in controls

procedure TSplashMain.SetText(text: string; color: cardinal = 0; progress: integer = 0);
begin
  if progress <> 0 then
    self.progress := progress;

  status.text := text;
  if (color <> 0) then
    spinner.filterwith(color);

  // if (progress <> 0) then
  // download.rect.width := (panel.rect.width div 100) * progress;
end;

constructor TSplashMain.create;
begin
  engine := TGUIEngine.create('');
  loadedbg := false;
  bgfading := false;

  panel := TGUIPanel.create(pointer(engine), Initializer.swidth - 325, Initializer.sheight - 96, 300, 42);
  panel.draggable := true;

  FileMan.FileSearch('media/gui/', 'download.png');
  FileMan.Load();

  download := TGUIImage.create(pointer(engine), 0, 0, 75, 42, 'download.png');
  download.setparent(pointer(panel));
  download.filterwith($20FFFFFF);
  download.rect.width := 0;

  spinner := TGUIImage.create(pointer(engine), 275, 20, 34, 34, 'spinner.png');
  spinner.SetRotable(true);
  spinner.filterwith($FFCC0064);
  spinner.setparent(pointer(panel));

  status := TGUIText.create(pointer(engine), 8, 22, 'Waiting for Strapper ..');
  status.setparent(pointer(panel));

  engine.pack;
end;

procedure TSplashMain.Logic();
begin
  if (download.rect.width < (panel.rect.width div 100) * self.progress) then
    download.rect.width := download.rect.width + 2;
  // do gravity, bounds checking etc.
  if (loadedbg) then
  begin
    if (Keyboard.GetAsyncKeyState(VK_LBUTTON) <> 0) then
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

    if (panel.rect.top + deltaforce.y < 0) or (panel.rect.top + panel.rect.height + deltaforce.y > Initializer.sheight)
    then
      deltaforce.y := deltaforce.y * -1;
    if (panel.rect.left + deltaforce.x < 0) or (panel.rect.left + panel.rect.width + deltaforce.x > Initializer.swidth)
    then
      deltaforce.x := deltaforce.x * -1;

    lastmouse := TMouse.CursorPos;
  end;

  spinner.rotation := spinner.rotation + 8;

  if not(loadedbg) and (GameImages.Resolve('background.png') > 0) then
  begin
    background := TGUIBackground.create(pointer(engine), 'background.png');
    engine.pack;
    loadedbg := true;
    background.fadein;
  end;

  engine.Logic;
end;

procedure TSplashMain.Draw();
begin
  engine.Draw;
end;

end.