unit GUI_Menu;

interface

// extension for the game interface.
// the backend should use UDP, the incoming UDP steam is threaded
// - we must be thread-safe.
{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
{$IFDEF Win32} Windows, ShellApi, {$ENDIF}
  SyncObjs, SysUtils, GUI_Engine, GUI_Element_Panel, GUI_Element_Button, GUI_Element_Edit, GUI_Element_Text,
  conf_protocol, System_Keyboard;

// read stats from player and display here...

type
  TGUIMenu = class
  public
    engine: TGUIEngine;
    procedure Logic();
    procedure Draw();
    procedure ServerSelect();
    procedure Logout();
  private
    frame: TGUIPanel;
    btnlogout, btnserver, btncharacter, btnoptions, btnwebsite, btncancel, btnexit: TGUIButton;
    menutext: TGUIText;
    cs: TCriticalSection;
    constructor Create;
    procedure CharacterSelect();
    procedure Exit();
    procedure Website();
    procedure Options();
  end;

procedure Initialize;

var
  MenuGUI: TGUIMenu;

implementation

uses Engine_Player, Conf_SpriteMap, Main, System_Initializer, network_worldserver, GUI_CharSelect, Network_MetaServer,
  GUI_StatusBox, GUI_LoginScreen, GUI_ServerSelect, Network_World;

procedure TGUIMenu.Logic();
begin
  if Keyboard.GetAsyncKeyState(VK_ESCAPE) <> 0 then
    frame.visible := true
  else
    frame.visible := false;

  if (frame.visible) then
    engine.Logic;
end;

procedure TGUIMenu.Draw();
begin
  if (frame.visible) then
    engine.Draw;
end;

procedure Initialize;
begin
  MenuGUI := TGUIMenu.Create;
end;

procedure TGUIMenu.CharacterSelect();
begin
  CharSelect.SetView;
  WorldConnection.SendDisconnect();
  worldcon.RequestCharacters;
end;

procedure TGUIMenu.Logout();
begin
  worldcon.client.Disconnect;
  LoginScreen.SetView();
end;

procedure TGUIMenu.ServerSelect();
begin
  worldcon.client.Disconnect;
  MetaCon.ListServers;
  ServerScreen.SetView();
end;

procedure TGUIMenu.Options();
begin
  //
end;

procedure TGUIMenu.Website();
begin
  //
end;

procedure TGUIMenu.Exit();
begin
  try
    worldcon.client.Disconnect;
  finally
    Main.Terminate;
  end;
end;

constructor TGUIMenu.Create;
var
  i: integer;
begin
  cs := TCriticalSection.Create;
  engine := TGUIEngine.Create('controls.png');

  frame := TGUIPanel.Create(Pointer(engine), trunc(Initializer.SWidth / 2) - 75, trunc(Initializer.Sheight / 2 - 102),
    150, 180);
  frame.draggable := true;
  frame.visible := false;

  menutext := TGUIText.Create(Pointer(engine), trunc(40), 5, 'System Menu');
  btnlogout := TGUIButton.Create(Pointer(engine), 10, 25, SPRITE_BTN, SPRITE_BTN_HOVER, 'Logout', Logout);
  btnserver := TGUIButton.Create(Pointer(engine), 10, 50, SPRITE_BTN, SPRITE_BTN_HOVER, 'Server Select', ServerSelect);
  btncharacter := TGUIButton.Create(Pointer(engine), 10, 75, SPRITE_BTN, SPRITE_BTN_HOVER, 'Character Select',
    CharacterSelect);
  btnoptions := TGUIButton.Create(Pointer(engine), 10, 100, SPRITE_BTN_DISABLED, SPRITE_BTN_DISABLED,
    'Options', Options);
  btnwebsite := TGUIButton.Create(Pointer(engine), 10, 125, SPRITE_BTN_DISABLED, SPRITE_BTN_DISABLED,
    'Website', Website);
  btnexit := TGUIButton.Create(Pointer(engine), 10, 150, SPRITE_BTN, SPRITE_BTN_HOVER, 'Exit', Exit);

  menutext.setparent(Pointer(frame));
  btnlogout.setparent(Pointer(frame));
  btnserver.setparent(Pointer(frame));
  btncharacter.setparent(Pointer(frame));
  btnoptions.setparent(Pointer(frame));
  btnwebsite.setparent(Pointer(frame));
  btnexit.setparent(Pointer(frame));

  engine.pack;
end;

end.
