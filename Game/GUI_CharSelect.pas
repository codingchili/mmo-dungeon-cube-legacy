unit GUI_CharSelect;

// display a warrior, thief, and mage.
// on hover select button, show some text with info about the class.
// if there is characterdata retrieved from the server, display it.
// level stats etc.

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses GUI_Engine, GUI_Element, GUI_Element_Background, GUI_element_textlines, GUI_element_panel, GUI_Element_Button,
  GUI_Element_Text, Controls, Mouse_Proxy,
  System_UtilExt,
  Conf_Protocol, SysUtils,
  GUI_Bars, {$IFDEF Win32}Windows, {$ENDIF} Classes;

type
  TLoadedProfession = (Thief, Warrior, Mage, None);

type
  TCharSelect = class
  private
    BG: TGUIBackground;
    war_btn, thief_btn, mag_btn, backbutton, exitbutton: TGUIButton;
    war_name, thief_name, mag_name: TGUIText;
    char_info, banner_panel: TGUIPanel;
    char_infotext, banner_text: TGUITextLines;
    char_classname: TGUIText;
    infoloaded: TLoadedProfession;
    procedure Back();
    procedure Exit();
    procedure MageSelect();
    procedure ThiefSelect();
    procedure Warriorselect();
    procedure SelectCharacter(profession: TProfession);
    procedure ClearCharacters();
  public
    engine: TGUIEngine;
    procedure Resized();
    procedure Draw();
    procedure Logic();
    procedure SetView();
    procedure AddCharacter(name, level: string; prof: TProfession);
    constructor create;
  end;

procedure Initialize;

var
  CharSelect: TCharSelect;

implementation

uses Main, System_initializer, Network_WorldServer, Engine_Player,
  network_world, Conf_Spritemap, GUI_StatusBox, GUI_Menu, GUI_Banner, GUI_Chat, GUI_Targeting;

procedure TCharSelect.SetView();
begin
  Statusbox.show('Loading Characters..');
  ClearCharacters();
  GUITargetFrame.clear;
  Main.View := TView.TCharacterSelect;
  GUIBanner.TextFromFile('data/banner_character.txt', -1);
end;

procedure TCharSelect.Back();
begin
  menugui.serverselect();
end;

procedure TCharSelect.Exit();
begin
  halt;
end;

procedure TCharSelect.Resized;
begin
  thief_btn.rect.top := trunc(initializer.sheight / 2) - 180;
  thief_btn.rect.left := trunc(initializer.swidth / 2) - 100;

  mag_btn.rect.top := trunc(initializer.sheight / 2) - 180;
  mag_btn.rect.left := trunc(initializer.swidth / 2) + 60;

  war_btn.rect.top := trunc(initializer.sheight / 2) - 180;
  war_btn.rect.left := trunc(initializer.swidth / 2) - 260;

  exitbutton.rect.top := initializer.sheight - 60;
  exitbutton.rect.left := initializer.swidth - 74;
  backbutton.rect.top := initializer.sheight - 60;
  backbutton.rect.left := initializer.swidth - 114;
end;

constructor TCharSelect.create;
begin
  engine := TGUIEngine.create('controls.png');

  BG := TGUIBackground.create(Pointer(engine), 'background.png');

  thief_btn := TGUIButton.create(Pointer(engine), trunc(initializer.swidth / 2) - 100,
    trunc(initializer.sheight / 2) - 180, BTN_WIDTH_PROFESSION, BTN_HEIGHT_PROFESSION, SPRITE_BTN_THIEF,
    SPRITE_BTN_HOVER_THIEF, '', ThiefSelect);

  mag_btn := TGUIButton.create(Pointer(engine), trunc(initializer.swidth / 2) + 60,
    trunc(initializer.sheight / 2) - 180, BTN_WIDTH_PROFESSION, BTN_HEIGHT_PROFESSION, SPRITE_BTN_MAGE,
    SPRITE_BTN_HOVER_MAGE, '', MageSelect);

  war_btn := TGUIButton.create(Pointer(engine), trunc(initializer.swidth / 2) - 260,
    trunc(initializer.sheight / 2) - 180, BTN_WIDTH_PROFESSION, BTN_HEIGHT_PROFESSION, SPRITE_BTN_WARRIOR,
    SPRITE_BTN_HOVER_WARRIOR, '', Warriorselect);

  war_name := TGUIText.create(Pointer(engine), 8, 140, 'Create');
  war_name.setparent(Pointer(war_btn));

  thief_name := TGUIText.create(Pointer(engine), 8, 140, 'Create');
  thief_name.setparent(Pointer(thief_btn));

  mag_name := TGUIText.create(Pointer(engine), 8, 140, 'Create');
  mag_name.setparent(Pointer(mag_btn));

  // ------------- char info -------------------------------------------------------------------
  char_info := TGUIPanel.create(Pointer(engine), round(initializer.swidth / 2 - (825 / 2)),
    round(initializer.sheight - 440), 825, 420);
  char_info.visible := false;

  char_infotext := TGUITextLines.create(Pointer(engine), 30, 45, 0);
  char_infotext.setparent(Pointer(char_info));
  infoloaded := TLoadedProfession.None;

  char_classname := TGUIText.create(Pointer(engine), 30, 12, 'The Mage');
  char_classname.setparent(Pointer(char_info));
  // --------------------------------------------------------------------------------------------

  // ------------- nav buttons -----------------------------------------------------------------
  exitbutton := TGUIButton.create(Pointer(engine), initializer.swidth - 74, initializer.sheight - 60,
    NAV_ICON_EXIT_ACTIVE, NAV_ICON_EXIT_HOVER, '', Exit);
  exitbutton.rect.width := NAV_ICON_SIZE;
  exitbutton.rect.height := NAV_ICON_SIZE;

  backbutton := TGUIButton.create(Pointer(engine), initializer.swidth - 114, initializer.sheight - 60,
    NAV_ICON_BACK_ACTIVE, NAV_ICON_BACK_HOVER, '', Back);
  backbutton.rect.width := NAV_ICON_SIZE;
  backbutton.rect.height := NAV_ICON_SIZE;
  // --------------------------------------------------------------------------------------------

  engine.pack;
end;

procedure TCharSelect.ClearCharacters;
begin
  mag_name.text := 'Create';
  thief_name.text := 'Create';
  thief_name.text := 'Create';
end;

// make profs dynamic
procedure TCharSelect.AddCharacter(name, level: string; prof: TProfession);
begin

  case (prof) of
    TProfession.Mage:
      mag_name.text := (name + ' lv.' + level);
    TProfession.Warrior:
      war_name.text := (name + ' lv.' + level);
    TProfession.Thief:
      thief_name.text := (name + ' lv.' + level);
  end;
end;

// onselect thief, onselectmag, onselectwar

procedure TCharSelect.SelectCharacter(profession: TProfession);
begin
  Statusbox.SetText('Loading Character ..');
  Statusbox.show;
  player.profession := profession;
  // ChatEngine.Clear; // dont clear ?
  worldcon.LoginCharacter(profession);
end;

procedure TCharSelect.ThiefSelect();
begin
  SelectCharacter(TProfession.Thief);
end;

procedure TCharSelect.Warriorselect();
begin
  SelectCharacter(TProfession.Warrior)
end;

procedure TCharSelect.MageSelect();
begin
  SelectCharacter(TProfession.Mage);
end;

procedure TCharSelect.Logic();
var
  hovering: boolean;
begin
  hovering := false;

  if (mag_btn.rect.Contains(Tmouse.CursorPos)) then
  begin
    if not(infoloaded = TLoadedProfession.Mage) then
    begin
      char_infotext.TextFromFile('data/bio_mage.txt');
      char_classname.text := 'The Mage';
      infoloaded := TLoadedProfession.Mage;
      char_info.rect.height := char_infotext.textheight + 52;
      char_info.rect.top := initializer.sheight - (char_infotext.textheight + 80);
    end;
    hovering := true;
  end;

  if (war_btn.rect.Contains(Tmouse.CursorPos)) then
  begin
    if not(infoloaded = TLoadedProfession.Warrior) then
    begin
      char_classname.text := 'The Warrior';
      char_infotext.TextFromFile('data/bio_warrior.txt');
      infoloaded := TLoadedProfession.Warrior;
      char_info.rect.height := char_infotext.textheight + 52;
      char_info.rect.top := initializer.sheight - (char_infotext.textheight + 80);
    end;
    hovering := true;
  end;

  if (thief_btn.rect.Contains(Tmouse.CursorPos)) then
  begin
    if not(infoloaded = TLoadedProfession.Thief) then
    begin
      char_classname.text := 'The Thief';
      char_infotext.TextFromFile('data/bio_thief.txt');
      infoloaded := TLoadedProfession.Thief;
      char_info.rect.height := char_infotext.textheight + 52;
      char_info.rect.top := initializer.sheight - (char_infotext.textheight + 80);
    end;
    hovering := true;
  end;

  if hovering = false then
  begin
    hovering := false;
    char_info.visible := false;
    infoloaded := TLoadedProfession.None;
  end
  else
  begin
    char_info.visible := true;
  end;

  engine.Logic;
end;

// drawevents
procedure TCharSelect.Draw();
begin
  engine.Draw;
end;

procedure Initialize();
begin
  CharSelect := TCharSelect.create;
end;

end.
