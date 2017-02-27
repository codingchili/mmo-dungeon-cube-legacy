unit GUI_Banner;

interface

uses SysUtils, GUI_Engine, GUI_Element_Panel, GUI_Element_TextLines, SyncObjs
{$IFDEF Win32}, Windows{$ELSE}, Types{$ENDIF};

const
  BANNER_WIDTH = 655;

type
  TGUIBanner = class
  private
    frame, framehint: TGUIPanel;
    textlines: TGUITextLines;
    timer: integer; // ttl for banner. ttl expires the alpha goes down. settext/loadtext refreshes alpha.
    procedure Center();
    procedure setPosition(pos: TPoint);
  public
    engine: TGUIEngine;
    procedure Logic;
    procedure Draw;
    procedure SetText(text: string; ttl: integer);
    procedure TextFromFile(filename: string; ttl: integer);
    procedure Hide();
    procedure Show();
    procedure Resized();

    constructor Create;
  end;

procedure Initialize;

var
  GUIBanner: TGUIBanner;

implementation

uses System_Log, Engine_Player, Main, System_Initializer, System_Camera, Vectors2, AsphyreTypes, Conf_Spritemap,
  Network_World, System_UtilExt, GUI_StatusBox, GUI_Chat;

procedure TGUIBanner.Show;
begin
  frame.Show;
end;

procedure TGUIBanner.Hide;
begin
  frame.Hide;
end;

procedure TGUIBanner.Resized;
begin
  Center();
end;

procedure TGUIBanner.setPosition(pos: TPoint);
begin
  frame.rect.Top := pos.Y;
  frame.rect.Left := pos.X;
end;

procedure TGUIBanner.Center();
begin
  frame.rect.height := textlines.textheight + 8;
  frame.rect.width := textlines.textwidth + 24;
  frame.rect.Left := round(Initializer.swidth / 2 - (frame.rect.width / 2));
end;

procedure TGUIBanner.SetText(text: string; ttl: integer);
begin
  textlines.SetText(text);
  Center();
  timer := ttl;
  frame.visible := true;
end;

procedure TGUIBanner.TextFromFile(filename: string; ttl: integer);
begin
  if textlines.TextFromFile(filename) then
  begin
    Center();
    timer := ttl;
    frame.visible := true;
  end;
end;

procedure TGUIBanner.Logic();
begin
  if frame.visible then
  begin
    if timer > 0 then
      dec(timer);

    if (timer = 0) then
    begin
      frame.visible := false;
    end;

    engine.Logic;
  end;
end;

procedure TGUIBanner.Draw();
begin
  if frame.visible then
    engine.Draw;
end;

procedure Initialize;
begin
  GUIBanner := TGUIBanner.Create;
end;

constructor TGUIBanner.Create;
var
  i: integer;
begin
  engine := TGUIEngine.Create('controls.png');

  frame := TGUIPanel.Create(Pointer(engine), trunc(Initializer.swidth / 2 - BANNER_WIDTH / 2), trunc(20),
    BANNER_WIDTH, 0);
  frame.draggable := true;
  frame.visible := false;

  textlines := TGUITextLines.Create(Pointer(engine), 12, 4, BANNER_WIDTH - 8, $FFFFFF00);
  textlines.setparent(Pointer(frame));

  engine.pack;
end;

end.
