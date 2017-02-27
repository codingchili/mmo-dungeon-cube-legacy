unit GUI_Inventory;

interface

uses GUI_Element_Text, GUI_Element_Button, GUI_Element_Panel, SyncObjs, SysUtils,
{$IFDEF Win32}Windows, {$ENDIF} GUI_Engine, conf_protocol,
  Classes, System_Keyboard, Controls;

type
  TGUIInventory = class
  public
    engine: TGUIEngine;
    procedure Logic;
    procedure Draw;
    procedure Refresh();
  private
    frame: TGUIPanel;
    health, energy, name, experience, INT, CON, DEX, STR, ACC, SPR, skillpts, statpts, attackpower, spellpower, mrdef,
      prdef: TGUIText;
    INT_ADD, CON_ADD, DEX_ADD, STR_ADD, ACC_ADD, SPR_ADD: TGUIButton;
    cs: TCriticalSection;
    constructor Create;
  end;

procedure Initialize;

var
  GUIInventory: TGUIInventory;
  togglelock: boolean;

implementation

uses System_Log, Engine_Player, Main, System_Initializer, System_Camera, Vectors2, AsphyreTypes, Conf_Spritemap,
  Network_World, System_UtilExt;

procedure TGUIInventory.Logic();
begin
  if not(frame.visible) then
  begin
    if (Keyboard.chars[VK_F2]) then
    begin
      Refresh;
      frame.show;
    end;
  end
  else if (Keyboard.chars[VK_F2]) then
    frame.Hide;

  engine.Logic;
end;

procedure TGUIInventory.Draw();
begin
  engine.Draw;
end;

procedure TGUIInventory.Refresh();
begin
  try
    name.text := player.name + ' lv.' + inttostr(player.level);
    health.text := 'HP: ' + inttostr(trunc(player.health)) + '/' + inttostr(trunc(player.maxhealth));
    energy.text := 'PW: ' + inttostr(trunc(player.energy)) + '/' + inttostr(trunc(player.maxenergy));
    experience.text := 'XP: ' + FormatFloat('0.##', (player.experience / player.maxexperience) * 100) + '%';
    INT.text := 'INT ' + inttostr(player.stat.INT);
    STR.text := 'STR ' + inttostr(player.stat.STR);
    CON.text := 'CON ' + inttostr(player.stat.CON);
    SPR.text := 'SPR ' + inttostr(player.stat.SPR);
    DEX.text := 'DEX ' + inttostr(player.stat.DEX);
    ACC.text := 'ACC ' + inttostr(player.stat.ACC);

    skillpts.text := 'Skill Points ' + inttostr(player.skillpts);
    statpts.text := 'Stat Points ' + inttostr(player.statpts);

    attackpower.text := 'Attack ' + inttostr(player.attackpower);
    spellpower.text := 'Spell   ' + inttostr(player.spellpower);
    mrdef.text := 'DEF ' + inttostr(player.prdef);
    prdef.text := 'MR ' + inttostr(player.mrdef);

    case (player.profession) of
      TProfession.Mage:
        energy.color := $FF009ACD;
      TProfession.Thief:
        energy.color := $FFFF00FF;
      TProfession.Warrior:
        energy.color := $FFFF6600;
    end;
  except
    // divide by zero?
  end;
end;

procedure Initialize;
begin
  GUIInventory := TGUIInventory.Create;
end;

constructor TGUIInventory.Create;
begin
  cs := TCriticalSection.Create;

  engine := TGUIEngine.Create('controls.png');

  frame := TGUIPanel.Create(Pointer(engine), 300, RenderForm.ClientHeight - 500, 375, 565);
  frame.draggable := true;
  frame.hide;

  name := TGUIText.Create(Pointer(engine), 10, 10, '');
  health := TGUIText.Create(Pointer(engine), 10, 40, '', $FFFF0000);
  energy := TGUIText.Create(Pointer(engine), 10, 56, '');
  experience := TGUIText.Create(Pointer(engine), 10, 72, '', $FFFFFF00);

  CON := TGUIText.Create(Pointer(engine), 20, 96, 'CON', $FF00FFFF);
  STR := TGUIText.Create(Pointer(engine), 20, 112, 'STR', $FF00FFFF);
  INT := TGUIText.Create(Pointer(engine), 20, 128, 'INT', $FF00FFFF);

  ACC := TGUIText.Create(Pointer(engine), 95, 96, 'ACC', $FF00FFFF);
  DEX := TGUIText.Create(Pointer(engine), 95, 112, 'DEX', $FF00FFFF);
  SPR := TGUIText.Create(Pointer(engine), 95, 128, 'SPR', $FF00FFFF);

  // (engine: Pointer; left, top, width, height: integer; sprite, fsprite: TPoint; text: string; OnClick: TOnCLick);

  INT_ADD := TGUIButton.Create(Pointer(engine), 5, 99, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));
  CON_ADD := TGUIButton.Create(Pointer(engine), 5, 115, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));
  DEX_ADD := TGUIButton.Create(Pointer(engine), 5, 131, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));
  STR_ADD := TGUIButton.Create(Pointer(engine), 80, 99, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));
  ACC_ADD := TGUIButton.Create(Pointer(engine), 80, 115, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));
  SPR_ADD := TGUIButton.Create(Pointer(engine), 80, 131, 10, 10, SPRITE_STAT_ADD, SPRITE_STAT_ADD_HOVER, '',
    TOnClick(NIL));

  attackpower := TGUIText.Create(Pointer(engine), 10, 157, 'Attack ', $FF00FFFF);
  spellpower := TGUIText.Create(Pointer(engine), 10, 173, 'Spell ', $FF00FFFF);
  mrdef := TGUIText.Create(Pointer(engine), 95, 157, 'DEF ', $FF00FFFF);
  prdef := TGUIText.Create(Pointer(engine), 95, 173, 'MR ', $FF00FFFF);

  skillpts := TGUIText.Create(Pointer(engine), 35, 199, 'Stat Points ', $FF00FFFF);
  statpts := TGUIText.Create(Pointer(engine), 35, 216, 'Skill Points ', $FF00FFFF);

  experience.setparent(Pointer(frame));
  name.setparent(Pointer(frame));
  health.setparent(Pointer(frame));
  energy.setparent(Pointer(frame));

  CON.setparent(Pointer(frame));
  INT.setparent(Pointer(frame));
  STR.setparent(Pointer(frame));
  DEX.setparent(Pointer(frame));
  ACC.setparent(Pointer(frame));
  SPR.setparent(Pointer(frame));

  CON_ADD.setparent(Pointer(frame));
  INT_ADD.setparent(Pointer(frame));
  STR_ADD.setparent(Pointer(frame));
  DEX_ADD.setparent(Pointer(frame));
  ACC_ADD.setparent(Pointer(frame));
  SPR_ADD.setparent(Pointer(frame));

  skillpts.setparent(Pointer(frame));
  statpts.setparent(Pointer(frame));

  attackpower.setparent(Pointer(frame));
  spellpower.setparent(Pointer(frame));
  mrdef.setparent(Pointer(frame));
  prdef.setparent(Pointer(frame));

  engine.pack;
end;

end.
