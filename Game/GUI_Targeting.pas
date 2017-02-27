unit GUI_Targeting;

// display a warrior, thief, and mage.
// on hover select button, show some text with info about the class.
// if there is characterdata retrieved from the server, display it.
// level stats etc.

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
{$IFDEF Win32} Windows, {$ENDIF}
  GUI_Engine, GUI_Element_Panel, GUI_Element_Text, GUI_Element_Image,
  System_UtilExt, Engine_Particles, Mouse_Proxy, Engine_player, Classes,
  SysUtils, System_Multiplayer;

type
  TGUITargetFrame = class
  private
    frame_bar, frame_hint: TGUIPanel;
    text_hint: TGUIText;
    hp_bar, power_bar, hp_bar_backing, power_bar_backing: TGUIImage;
  public
    targetid: integer;
    character_name, character_level: TGUIText;
    engine: TGUIEngine;
    procedure Draw();
    procedure Logic();
    constructor create();
    procedure clearTarget(id: integer);
    procedure clear();
  end;

procedure Initialize;

var
  GUITargetFrame: TGUITargetFrame;

implementation

uses Main, System_Initializer, Network_MetaServer, System_Keyboard, Conf_Protocol,
  Conf_Spritemap, System_Camera;

procedure TGUITargetFrame.clear;
begin
  targetid := -1;
end;

procedure TGUITargetFrame.clearTarget(id: integer);
begin
  if (targetid = id) then
    targetid := -1;
end;

constructor TGUITargetFrame.create;
begin
  engine := TGUIEngine.create('controls.png');

  frame_bar := TGUIPanel.create(Pointer(engine), initializer.swidth div 2 - 150, 10, 300, 46);
  frame_bar.draggable := true;
  frame_bar.border := $44FF0000;

  character_name := TGUIText.create(Pointer(engine), 5, 3, '[Character::getName();]');
  character_level := TGUIText.create(Pointer(engine), 300 - 42, 3, '');

  hp_bar := TGUIImage.create(Pointer(engine), 2, 18, 0, BAR_HEIGHT_FILL, SPRITE_HPBAR);
  hp_bar_backing := TGUIImage.create(Pointer(engine), 2, 18, BAR_WIDTH, BAR_HEIGHT, SPRITE_BARFRAME);

  power_bar := TGUIImage.create(Pointer(engine), 2, 31, 0, BAR_HEIGHT_FILL, SPRITE_FRBAR);
  power_bar_backing := TGUIImage.create(Pointer(engine), 2, 31, BAR_WIDTH, BAR_HEIGHT, SPRITE_BARFRAME);

  frame_hint := TGUIPanel.create(Pointer(engine), 0, 0, 75, 22);
  frame_hint.draggable := false;
  frame_hint.Hide;

  text_hint := TGUIText.create(Pointer(engine), 6, 4, 'Error #G1', $FFFFFFFF);
  text_hint.setparent(frame_hint);

  hp_bar.setparent(Pointer(frame_bar));
  hp_bar_backing.setparent(Pointer(frame_bar));

  power_bar.setparent(Pointer(frame_bar));
  power_bar_backing.setparent(Pointer(frame_bar));

  character_name.setparent(Pointer(frame_bar));
  character_level.setparent(Pointer(frame_bar));

  targetid := -1;
end;

// check the player profession and change the bar type if neccessary
procedure TGUITargetFrame.Logic();
var
  i: integer;
  netplayer: TNetPlayer;
begin
  if keyboard.GetAsyncKeyState(VK_ESCAPE) <> 0 then
    targetid := -1;
  // get target by mouse click
  if (keyboard.getasynckeystate(VK_LBUTTON) <> 0) then
  begin

    multiplayer.lock;
    for i := 0 to length(multiplayer.player) - 1 do
    begin
      with (multiplayer.player[i]) do
      begin
        if (TPlayer.getBounds(trunc(X), trunc(Y), cam.X, cam.Y, player.size))
          .contains(Point(Tmouse.CursorPos.X, Tmouse.CursorPos.Y)) then
        begin
          targetid := aID;

          case (multiplayer.player[i].profession) of
            TProfession.Mage:
              power_bar.sprite := SPRITE_MPBAR;
            TProfession.Thief:
              power_bar.sprite := SPRITE_ENBAR;
            TProfession.Warrior:
              power_bar.sprite := SPRITE_FRBAR;
          end;
        end;
      end;
    end;
    multiplayer.unlock;
  end;

  netplayer := multiplayer.getplayer(targetid);
  if (targetid < 0) or (netplayer.hp <= 0) then
    frame_bar.Hide
  else
  begin
    frame_bar.Show;

    character_level.text := 'lv.' + inttostr(netplayer.level);
    character_name.text := netplayer.name;

    hp_bar.rect.width := trunc((netplayer.hp / netplayer.hpmax) * BAR_WIDTH);
    power_bar.rect.width := trunc((netplayer.energy / netplayer.energymax) * BAR_WIDTH);

    if (hp_bar_backing.GetRect.contains(Tmouse.CursorPos)) then
    begin
      frame_hint.visible := true;
      text_hint.text := inttostr(round(netplayer.hp)) + '/' + inttostr(round(netplayer.hpmax));
    end
    else if (power_bar_backing.GetRect.contains(Tmouse.CursorPos)) then
    begin
      frame_hint.visible := true;
      text_hint.text := inttostr(round(netplayer.energy)) + '/' + inttostr(round(netplayer.energymax));
    end
    else
      frame_hint.visible := false; // hide;

    frame_hint.rect.top := Tmouse.CursorPos.Y - 12;
    frame_hint.rect.Left := Tmouse.CursorPos.X - text_hint.getWidth - 6;
    frame_hint.rect.width := text_hint.getWidth() + 12;
  end;

  engine.Logic;
end;

// drawevents
procedure TGUITargetFrame.Draw();
begin
  engine.Draw;
end;

procedure Initialize();
begin
  GUITargetFrame := TGUITargetFrame.create;
end;

end.