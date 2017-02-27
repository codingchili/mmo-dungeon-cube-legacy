unit GUI_LoginScreen;

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF Win32} Windows, {$ENDIF}
  GUI_Engine, GUI_element, GUI_Element_Edit, GUI_Element_Background,
  GUI_Element_Text, GUI_Element_Button, GUI_Element_Image,
  GUI_Element_Panel, Controls, System_UtilExt, SysUtils, Classes, Mouse_Proxy;

const
  FRAME_WIDTH: integer = 300;
  FRAME_HEIGHT: integer = 250;
  REGISTER_TEXT = 'Register New Account!';
  LOGIN_HINT = 'Enter Username & Password.';
  REGISTER_HINT = 'Enter Username, Password & Serial.';
  USER_REQ = 'Username: 6-16 Characters, [a-Z, 0-9]';
  PASS_REQ = 'Password: 12-42 Characters, [a-Z, 0-9, !?#%&[](),.-''~]';
  LICENSE_REQ = 'License: 25-29 Characters, [a-Z, 0-9, -+]';

type
  TLoginScreen = class
  private
    BG: TGUIBackground;
    reglink: TGUIText;
    loginbutton, exitbutton: TGUIButton;
    backimage: TGUIImage;
    frame, requirements: TGUIPanel;
    tablock, enterlock: boolean;
    hint, requirements_text: TGUIText;
    procedure Exit();
    procedure ShowRegister();
    procedure Login();
    procedure ShowRequirements();
    procedure HideRegister();
  public
    engine: TGUIEngine;
    passfield, textfield, newkey: TGUIEdit;
    constructor create();
    procedure SetView();
    procedure Draw();
    procedure Logic();
    procedure Resized();
  end;

procedure Initialize;

var
  LoginScreen: TLoginScreen;

implementation

uses Main, Conf_Protocol, System_Initializer, Network_MetaServer,
  System_Keyboard,
{$IFDEF Win32}System_Audio, {$ENDIF}
  Conf_Spritemap, System_Log, GUI_StatusBox, GUI_Banner, GUI_License;

procedure TLoginScreen.Resized;
begin
  // reposition..
  frame.rect.left := trunc(initializer.swidth / 2 - FRAME_WIDTH / 2);
  frame.rect.top := trunc(initializer.sheight / 2 - FRAME_HEIGHT / 2);

  exitbutton.rect.top := initializer.sheight - 60;
  exitbutton.rect.left := initializer.swidth - 74;
  backimage.rect.top := initializer.sheight - 60;
  backimage.rect.left := initializer.swidth - 114;
end;

procedure TLoginScreen.SetView;
begin
  GUIBanner.TextFromFile('data/banner_login.txt', -1);
  HideRegister;
  Main.View := TView.TLogin;
end;

procedure TLoginScreen.Exit();
begin
{$IFDEF Win32}
  ExitProcess(0);
{$ENDIF}
{$IFDEF Linux}
  Halt;
{$ENDIF}
end;

procedure TLoginScreen.Login();
begin
  StatusBox.settext('Connecting ..');
  StatusBox.show;

  if (newkey.visible) then
    MetaCon.Login(textfield.text, passfield.text, newkey.text)
  else
    MetaCon.Login(textfield.text, passfield.text);
end;

procedure TLoginScreen.HideRegister();
begin
  newkey.Hide;
  hint.text := LOGIN_HINT;
  hint.rect.left := round(FRAME_WIDTH / 2 - GameFonts[0].TextWidth(LOGIN_HINT) / 2);
  loginbutton.SetCaption('Login');
  GUIBanner.TextFromFile('data/banner_login.txt', -1);
end;

procedure TLoginScreen.ShowRegister();
begin
  newkey.show;
  passfield.focus := false;
  textfield.focus := true;
  newkey.focus := false;

  hint.text := REGISTER_HINT;
  hint.rect.left := round(FRAME_WIDTH / 2 - GameFonts[0].TextWidth(REGISTER_HINT) / 2);
  loginbutton.SetCaption('Register');
  GUIBanner.TextFromFile('data/banner_register.txt', -1);
end;

procedure TLoginScreen.Logic();
begin
  if Keyboard.chars[VK_LBUTTON] then
  begin
    if (reglink.GetRect.Contains(Tmouse.CursorPos)) then
      ShowRegister();
  end;

  if (Keyboard.GetAsyncKeyState(VK_ESCAPE) = KEYBOARD_PRESS) then
    HideRegister();

  ShowRequirements();

  engine.Logic;
end;

procedure TLoginScreen.Draw();
begin
  if licensebox.frame.visible then
    GUIBanner.Hide
  else
    GUIBanner.show;

  engine.Draw;
end;

procedure TLoginScreen.ShowRequirements();
begin
  requirements.visible := false;

  if (newkey.visible) then
    if (textfield.focus) then
    begin
      requirements.visible := true;
      requirements_text.text := USER_REQ;
      requirements.rect.width := round(GameFonts[0].TextWidth(USER_REQ)) + 4;
      requirements.rect.left := textfield.GetRect.left + 295;
      requirements.rect.top := textfield.GetRect.top;
    end
    else if (passfield.focus) then
    begin
      requirements.visible := true;
      requirements_text.text := PASS_REQ;
      requirements.rect.width := round(GameFonts[0].TextWidth(PASS_REQ)) + 4;
      requirements.rect.left := passfield.GetRect.left + 295;
      requirements.rect.top := passfield.GetRect.top;
    end
    else if (newkey.focus) then
    begin
      requirements.visible := true;
      requirements.rect.width := round(GameFonts[0].TextWidth(LICENSE_REQ)) + 4;
      requirements_text.text := LICENSE_REQ;
      requirements.rect.left := newkey.GetRect.left + 295;
      requirements.rect.top := newkey.GetRect.top;
    end;

end;

constructor TLoginScreen.create;
begin
  engine := TGUIEngine.create('controls.png');
  enterlock := false;
  tablock := false;

  BG := TGUIBackground.create(pointer(engine), 'background.png');

  frame := TGUIPanel.create(pointer(engine), trunc(initializer.swidth / 2 - FRAME_WIDTH / 2),
    trunc(initializer.sheight / 2 - FRAME_HEIGHT / 2), FRAME_WIDTH, FRAME_HEIGHT);

  loginbutton := TGUIButton.create(pointer(engine), trunc(FRAME_WIDTH / 2 - BUTTON_WIDTH / 2), 170, SPRITE_BTN,
    SPRITE_BTN_HOVER, 'Login', Login);
  loginbutton.setparent(pointer(frame));
  loginbutton.SetDefault;

  // ------------- input data -----------------------------
  requirements := TGUIPanel.create(pointer(engine), 300, 300, 250, 20);
  requirements_text := TGUIText.create(pointer(engine), 2, 2, '', $FFFFFF00);
  requirements_text.setparent(pointer(requirements));
  requirements.visible := false;
  // --------------------------------------------------------

  // ------------ nav panel ----------------
  exitbutton := TGUIButton.create(pointer(engine), initializer.swidth - 74, initializer.sheight - 60,
    NAV_ICON_EXIT_ACTIVE, NAV_ICON_EXIT_HOVER, '', self.Exit);
  exitbutton.rect.width := NAV_ICON_SIZE;
  exitbutton.rect.height := NAV_ICON_SIZE;

  backimage := TGUIImage.create(pointer(engine), initializer.swidth - 114, initializer.sheight - 60, NAV_ICON_SIZE,
    NAV_ICON_SIZE, NAV_ICON_BACK_DISABLED);
  // --------------------------------------

  reglink := TGUIText.create(pointer(engine), round(FRAME_WIDTH / 2 - GameFonts[0].TextWidth(REGISTER_TEXT) / 2), 210,
    REGISTER_TEXT, $FF00FFFF, $FFFF0000);
  // reglink.color := $FF00FFFF;
  reglink.setparent(pointer(frame));

  hint := TGUIText.create(pointer(engine), round(FRAME_WIDTH / 2 - GameFonts[0].TextWidth(LOGIN_HINT) / 2), 20,
    LOGIN_HINT, $FFFFFFFF, $FFFFFFFF);
  hint.setparent(pointer(frame));

  passfield := TGUIEdit.create(pointer(engine), trunc(FRAME_WIDTH / 2 - INPUTFIELD_WIDTH / 2), 80, SPRITE_INPUTFIELD,
    SPRITE_INPUTFIELD_FOCUS);
  passfield.text := Protocol.pass;
  passfield.maxlen := 25;
  passfield.mask := true;
  passfield.setparent(pointer(frame));

  newkey := TGUIEdit.create(pointer(engine), trunc(FRAME_WIDTH / 2 - INPUTFIELD_WIDTH / 2), 110, SPRITE_INPUTFIELD,
    SPRITE_INPUTFIELD_FOCUS);
  newkey.text := Protocol.serial;
  newkey.maxlen := 29;
  newkey.Hide;
  newkey.setparent(pointer(frame));

  textfield := TGUIEdit.create(pointer(engine), trunc(FRAME_WIDTH / 2 - INPUTFIELD_WIDTH / 2), 50, SPRITE_INPUTFIELD,
    SPRITE_INPUTFIELD_FOCUS);
  textfield.text := Protocol.user;
  textfield.maxlen := 25;
  textfield.focus := true;
  textfield.setparent(pointer(frame));

  newkey.text := Protocol.serial;
  textfield.text := Protocol.user;
  passfield.text := Protocol.pass;

  engine.pack;
end;

procedure Initialize();
begin
  LoginScreen := TLoginScreen.create;
end;

end.
