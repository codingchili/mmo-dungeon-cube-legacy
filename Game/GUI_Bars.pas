unit GUI_Bars;

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
  SysUtils;

type
  TGUIBar = class
  private
    frame_bar, frame_hint: TGUIPanel;
    text_hint: TGUIText;
    hp_bar, xp_bar, power_bar, hp_bar_backing, power_bar_backing, xp_bar_backing, minibar, minibar_back: TGUIImage;
  public
    character_name, character_level: TGUIText;
    engine: TGUIEngine;
    procedure Draw();
    procedure Logic();
    constructor create();
  end;

procedure Initialize;

var
  GUIBar: TGUIBar;

implementation

uses Main, System_Initializer, Network_MetaServer, Keyboard, Conf_Protocol,
  Conf_Spritemap, System_Camera, System_Multiplayer, GUI_Targeting;

constructor TGUIBar.create;
begin
  engine := TGUIEngine.create('controls.png');

  frame_bar := TGUIPanel.create(Pointer(engine), 10, 10, 300, 59);
  frame_bar.draggable := true;

  character_name := TGUIText.create(Pointer(engine), 5, 3, '');
  character_level := TGUIText.create(Pointer(engine), 300 - 42, 3, '');

  hp_bar := TGUIImage.create(Pointer(engine), 2, 18, 0, BAR_HEIGHT_FILL, SPRITE_HPBAR);
  hp_bar_backing := TGUIImage.create(Pointer(engine), 2, 18, BAR_WIDTH, BAR_HEIGHT, SPRITE_BARFRAME);

  power_bar := TGUIImage.create(Pointer(engine), 2, 31, 0, BAR_HEIGHT_FILL, SPRITE_FRBAR);
  power_bar_backing := TGUIImage.create(Pointer(engine), 2, 31, BAR_WIDTH, BAR_HEIGHT, SPRITE_BARFRAME);

  xp_bar := TGUIImage.create(Pointer(engine), 2, 44, 0, BAR_HEIGHT_FILL, SPRITE_XPBAR);
  xp_bar_backing := TGUIImage.create(Pointer(engine), 2, 44, BAR_WIDTH, BAR_HEIGHT, SPRITE_BARFRAME);

  minibar := TGUIImage.create(Pointer(engine), 0, 1, 43, 3, SPRITE_MINIBARFILL);
  minibar_back := TGUIImage.create(Pointer(engine), 0, 0, 43, 5, SPRITE_MINIBAR);

  frame_hint := TGUIPanel.create(Pointer(engine), 0, 0, 75, 22);
  frame_hint.draggable := false;

  text_hint := TGUIText.create(Pointer(engine), 6, 4, 'Error #G1', $FFFFFFFF);
  text_hint.setparent(frame_hint);

  hp_bar.setparent(Pointer(frame_bar));
  hp_bar_backing.setparent(Pointer(frame_bar));

  xp_bar.setparent(Pointer(frame_bar));
  xp_bar_backing.setparent(Pointer(frame_bar));

  power_bar.setparent(Pointer(frame_bar));
  power_bar_backing.setparent(Pointer(frame_bar));

  character_name.setparent(Pointer(frame_bar));
  character_level.setparent(Pointer(frame_bar));
  minibar.setparent(Pointer(minibar_back));

  // engine.pack;
end;

// check the player profession and change the bar type if neccessary
procedure TGUIBar.Logic();
var
  i: integer;
begin

  case (player.profession) of
    TProfession.Mage:
      power_bar.sprite := SPRITE_MPBAR;
    TProfession.Thief:
      power_bar.sprite := SPRITE_ENBAR;
    TProfession.Warrior:
      power_bar.sprite := SPRITE_FRBAR;
  end;

  character_level.text := 'lv.' + inttostr(player.level);

  hp_bar.rect.width := trunc((player.health / player.maxhealth) * BAR_WIDTH);
  power_bar.rect.width := trunc((player.energy / player.maxenergy) * BAR_WIDTH);
  xp_bar.rect.width := trunc((player.experience / player.maxexperience) * BAR_WIDTH);

  if (hp_bar_backing.GetRect.contains(Tmouse.CursorPos)) then
  begin
    frame_hint.visible := true;
    text_hint.text := inttostr(round(player.health)) + '/' + inttostr(round(player.maxhealth));
  end
  else if (power_bar_backing.GetRect.contains(Tmouse.CursorPos)) then
  begin
    frame_hint.visible := true;
    text_hint.text := inttostr(round(player.energy)) + '/' + inttostr(round(player.maxenergy));
  end
  else if (xp_bar_backing.GetRect.contains(Tmouse.CursorPos)) then
  begin
    frame_hint.visible := true;
    text_hint.text := FormatFloat('0.##', (player.experience / player.maxexperience) * 100) + '%';
  end
  else
    frame_hint.visible := false; // hide;

  frame_hint.rect.top := Tmouse.CursorPos.Y - 12;
  frame_hint.rect.Left := Tmouse.CursorPos.X - text_hint.getWidth - 6;
  frame_hint.rect.width := text_hint.getWidth() + 12;

  if (player.bounds(player, trunc(cam.X), trunc(cam.Y)).contains(Point(Tmouse.CursorPos.X, Tmouse.CursorPos.Y))) then
  begin
    minibar.rect.width := trunc((player.health / player.maxhealth) * 43);
    minibar_back.show;
    minibar.show;
    minibar_back.rect.top := round(player.Y + cam.Y) - 40;
    minibar_back.rect.Left := round(player.X + cam.X) - 20;
    // minibar_back.pos.top := round(player.y + cam.y) - 40;
    // minibar_back.pos.left := round(player.X + cam.X) - 20;
  end
  else
  begin
    minibar_back.hide;
    minibar.hide;
  end;

  Multiplayer.Lock;
  for i := 0 to high(Multiplayer.player) do
  begin
    with (Multiplayer.player[i]) do
    begin
      if (TPlayer.getBounds(trunc(X), trunc(Y), cam.X, cam.Y, player.size))
        .contains(Point(Tmouse.CursorPos.X, Tmouse.CursorPos.Y)) or (Multiplayer.player[i].aID = GUITargetFrame.targetid)
      then
      begin
        if hp < 0 then
          Multiplayer.player[i].hp := 0;

        minibar.rect.width := trunc((hp / hpmax) * 43);
        minibar_back.rect.top := round(Y + cam.Y) - 40;
        minibar_back.rect.Left := round(X + cam.X) - 20;
        minibar.show;
        minibar_back.show;
      end;
    end;
  end;
  Multiplayer.Unlock;

  engine.Logic;
end;

// drawevents
procedure TGUIBar.Draw();
begin
  engine.Draw;
end;

procedure Initialize();
begin
  GUIBar := TGUIBar.create;
end;

end.
