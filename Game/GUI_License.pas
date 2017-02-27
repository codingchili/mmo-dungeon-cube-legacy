unit GUI_License;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses Rectarea, GUI_Engine, GUI_Element_Panel, GUI_Element, GUI_Element_Text, GUI_Element_TextLines, GUI_element_Button,
  Controls,
  System_UtilExt, SysUtils, Classes, Conf_Spritemap, Dialogs, Vectors2, AsphyreTypes;

type
  TLicenseDialog = class
  private
    textbox: TGUIPanel;
    accept, decline, close: TGUIButton;
    header: TGUIText;
    textlines: TGUITextLines;
    onclick: TOnClick;
    procedure PatchNotes();
    procedure Setheader(text: string);
    procedure exit();
  public
    engine: TGUIEngine;
    frame: TGUIPanel;
    procedure show();
    procedure hide();
    procedure Draw();
    procedure Logic();
    constructor create;
  end;

procedure Initialize;

var
  licensebox: TLicenseDialog;

implementation

uses Network_WorldServer, System_Log, System_Initializer, Main, GUI_LoginScreen, Effects_Blending;

procedure TLicenseDialog.exit;
begin
  halt;
end;

constructor TLicenseDialog.create;
begin
  engine := TGUIEngine.create('controls.png');

  frame := TGUIPanel.create(Pointer(engine), trunc(Initializer.swidth / 2) - 264, trunc(Initializer.sheight / 2) - 329,
    528, 658);
  frame.draggable := true;

  if (Initializer.update or Initializer.license) then
    frame.show
  else
    frame.hide;

  textbox := TGUIPanel.create(Pointer(engine), 20, 45, 488, 570);
  textbox.setparent(Pointer(frame));

  header := TGUIText.create(Pointer(engine), 0, 16, 'header', $FFFFFFFF);
  header.setparent(Pointer(frame));

  accept := TGUIButton.create(Pointer(engine), trunc(528 - BUTTON_WIDTH - 125), trunc(658 - BUTTON_HEIGHT - 12),
    SPRITE_BTN, SPRITE_BTN_HOVER, 'Accept', hide);
  accept.setparent(Pointer(frame));
  accept.SetDefault;

  decline := TGUIButton.create(Pointer(engine), trunc(125), trunc(658 - BUTTON_HEIGHT - 12), SPRITE_BTN,
    SPRITE_BTN_HOVER, 'Decline', exit);
  decline.setparent(Pointer(frame));

  close := TGUIButton.create(Pointer(engine), trunc((528 / 2) - (BUTTON_WIDTH / 2)), trunc(658 - BUTTON_HEIGHT - 12),
    SPRITE_BTN, SPRITE_BTN_HOVER, 'Close', hide);
  close.hide;
  close.setparent(Pointer(frame));
  close.SetDefault;

  textlines := TGUITextLines.create(Pointer(engine), 26, 52, 80, $FFBBBBBB);
  textlines.setparent(Pointer(frame));


  if (Initializer.license) then
  begin
    Setheader('License Agreement & Terms of Use.');

    if (Initializer.update) then
      accept.onclick := PatchNotes;

    if not textlines.TextFromFile('conf/License.txt') then
      if (Initializer.update) then
        PatchNotes()
      else
        frame.hide;
  end;

  if (Initializer.update) and not(Initializer.license) then
    PatchNotes();

  engine.pack;
end;

// license was accepted.
procedure TLicenseDialog.PatchNotes();
begin
  Setheader('Patch Notes for Version ' + Initializer.version + ' - ' + Main.BUILD + '.');

  if textlines.TextFromFile('conf/Patch.txt') then
  begin
    frame.show;
    decline.hide;
    accept.hide;
    close.show;
  end
  else
    frame.visible := false;
end;

procedure TLicenseDialog.show();
begin
  try
    frame.show;
  except
    On E: Exception do
      showmessage(E.Message);
  end;
end;

procedure TLicenseDialog.Setheader(text: string);
begin
  header.rect.SetLocation(round(528 / 2 - GameFonts[0].TextWidth(text) / 2), header.rect.top);
  header.text := text;
end;

procedure TLicenseDialog.hide();
begin
  frame.hide;
end;

procedure TLicenseDialog.Logic();
begin
  engine.Logic();
end;

procedure TLicenseDialog.Draw();
begin
  if frame.visible then
    engine.Draw;
end;

procedure Initialize();
begin
  licensebox := TLicenseDialog.create;
end;

end.
