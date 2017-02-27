unit GUI_StatusBox;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses
{$IFDEF Win32} Windows, {$ENDIF}
  GUI_Engine, GUI_Element_Panel, GUI_Element_Edit, GUI_Element_Image, GUI_Element_TextLines,
  GUI_Element_Text, GUI_Element_Button, Controls, System_UtilExt,
  SysUtils, Classes, Conf_Spritemap, System_Initializer, Dialogs, SyncObjs;

const
  ONE_FRAME = 18;

type
  TStatusDialog = class
  private
    onclick: TOnClick;
    textpos: Tpoint;
    enterlock: boolean;
    textlines: TGUITextLines;
    gifspinner: TGUIImage;
    working: boolean;
    cs: TCriticalSection;
  public
    button: TGUIButton;
    field: TGUIedit;
    frame: TGUIPanel;
    engine: TGUIEngine;
    procedure Terminate();
    procedure show(text: string = ''; beginwork: boolean = false);
    procedure hide();
    procedure Draw();
    procedure Logic();
    procedure finished(onclick: TOnClick = NIL);
    procedure settext(text: string);
    procedure Resized();
    constructor create;
  end;

procedure Initialize;

var
  statusbox: TStatusDialog;

implementation

uses Network_WorldServer, System_Log, AsphyreTypes, Vectors2;

procedure TStatusDialog.Resized;
begin
  frame.rect.left := initializer.swidth div 2 - 281;
  frame.rect.top := initializer.sheight div 2 - 136;
end;

constructor TStatusDialog.create;
begin
  cs := TCriticalSection.create;
  engine := TGUIEngine.create('controls.png');

  frame := TGUIPanel.create(Pointer(engine), trunc(initializer.swidth / 2) - 281, trunc(initializer.sheight / 2) - 136,
    562, 234);
  frame.hide;
  frame.draggable := true;

  button := TGUIButton.create(Pointer(engine), trunc(562 / 2 - BUTTON_WIDTH / 2),
    trunc(254 / 2 - BUTTON_HEIGHT / 2 + 65), SPRITE_BTN, SPRITE_BTN_HOVER, 'Close', hide);
  button.hide;
  button.SetDefault;

  gifspinner := TGUIImage.create(Pointer(engine), frame.rect.width div 2, button.rect.top - 64, 34, 34,
    SPRITE_GIFSPINNER);
  gifspinner.setparent(Pointer(frame));
  gifspinner.SetRotable(true);

  field := TGUIedit.create(Pointer(engine), trunc(562 / 2 - INPUTFIELD_WIDTH / 2), trunc(254 / 2) - 10,
    SPRITE_INPUTFIELD, SPRITE_INPUTFIELD_FOCUS);
  field.hide;

  textlines := TGUITextLines.create(Pointer(engine), 100, 80, 80, $FFFFFFFF);
  textlines.setparent(Pointer(frame));

  button.setparent(Pointer(frame));
  field.setparent(Pointer(frame));
  // text.setparent(Pointer(frame));

  engine.pack;
end;

procedure TStatusDialog.Terminate;
begin
{$IFDEF Win32}ExitProcess(0); {$ELSE}
  Halt; {$ENDIF}
end;

procedure TStatusDialog.finished(onclick: TOnClick = NIL);
begin
  cs.Release;
  gifspinner.hide;

  if (@onclick <> NIL) then
    button.onclick := onclick
  else
    button.onclick := hide;

  button.show;
  cs.Release;
end;

procedure TStatusDialog.show(text: string = ''; beginwork: boolean = false);
begin
  cs.Acquire;
  gifspinner.show;
  try
    button.setcaption('Close');
    button.hide;
    frame.show;
  except
    // showmessage('ERROR IN SETVISIBILITY!');
  end;
  cs.Release;

  if text <> '' then
    settext(text);
end;

procedure TStatusDialog.hide();
begin
  cs.Acquire;
  frame.hide;
  button.hide;
  cs.Release;
end;

procedure TStatusDialog.settext(text: string);
begin
  cs.Acquire;
  textlines.settext(text);
  textlines.rect.left := round(562 / 2 - (textlines.textwidth / 2));
  cs.Release;
  //sleep(ONE_FRAME);
end;

procedure TStatusDialog.Logic();
begin
  cs.Acquire;
  gifspinner.rotation := gifspinner.rotation + 8;
  engine.Logic();
  cs.Release;
end;

procedure TStatusDialog.Draw();
begin
  cs.Acquire;
  engine.Draw;
  cs.Release;
end;

procedure Initialize();
begin
  statusbox := TStatusDialog.create;
end;

end.
