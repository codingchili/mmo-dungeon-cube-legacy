unit GUI_Element_Edit;

interface

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}
                                                                                // clipboard not licensed yet.
uses
 {$IFDEF Win32} Windows, {$ENDIF}
  Rectarea, Controls, GUI_Element, GUI_Engine, GUI_Element_Text, Vectors2, AsphyreTypes, AbstractCanvas, {ClipBoard,}
  SysUtils, Classes, Types, Mouse_Proxy, GUI_TextFilter;

const
  INPUTFIELD_WIDTH = 260;
  INPUTFIELD_HEIGHT = 22;
  INPUTFIELD_MAXLEN = 80;

type
  TGUIEdit = class(TGUIElement)
  private
    rect: Rectarea.TRect;
    sprite, fsprite: TPoint;
    color: cardinal;
    field: TGUIText;
  public
    text: string; // gettext function returns output
    mask: boolean;
    focus: boolean;
    maxlen: integer;
    procedure Draw(); override;
    procedure Logic(); override;
    procedure Hide(); override;
    procedure MouseEvent(var event: TGUIMouseEvent); override;
    procedure KeyEvent(var event: TGUIKeyEvent); override;
    constructor Create(engine: Pointer; left, top: integer; sprite, fsprite: TPoint; text: string = ''); overload;
  end;

  // (@engine, 8, 137, SPRITE_INPUTFIELD_DISCRETE, SPRITE_INPUTFIELD_FOCUS, '');

implementation

uses System_Initializer, System_Keyboard{$IFDEF Win32} , System_Audio{$ENDIF};

{ TGUIText }

constructor TGUIEdit.Create(engine: Pointer; left, top: integer; sprite, fsprite: TPoint; text: string = '');
begin
  inherited Create(engine, left, top, INPUTFIELD_WIDTH, INPUTFIELD_HEIGHT);
  self.mode := Mode_Edit; // engine identifier
  self.text := text;
  self.color := TEXT_COLOR;
  self.sprite := sprite;
  self.fsprite := fsprite;
  maxlen := INPUTFIELD_MAXLEN;
  rect := self.GetRect;

  field := TGUIText.Create(engine, 5, 3, text, TEXT_COLOR);
  field.setparent(Pointer(self));

  TGUIEngine(engine).Add(TGUIElement(self));
end;

procedure TGUIEdit.Hide;
begin
  if (focus) then
    TGUIEngine(engine).TabEvent(TGUIElement(self));

  visible := false;
end;

procedure TGUIEdit.Draw;
begin
  if (focus) then
    GameCanvas.UseImagePx(GameImages.items[spritesheet], Point4(fsprite.x, fsprite.y, rect.width + fsprite.x, fsprite.y,
      rect.width + fsprite.x, rect.height + fsprite.y, fsprite.x, rect.height + fsprite.y))
  else
    GameCanvas.UseImagePx(GameImages.items[spritesheet], Point4(sprite.x, sprite.y, rect.width + sprite.x, sprite.y,
      rect.width + sprite.x, rect.height + sprite.y, sprite.x, rect.height + sprite.y));

  GameCanvas.TexMap(pBounds4(rect.left, rect.top, rect.width, rect.height), cColor4($FFFFFFFF), TBlendingEffect.beNormal);
end;

procedure TGUIEdit.MouseEvent(var event: TGUIMouseEvent);
begin
  if (event = TGUIMouseEvent.left_down) and not rect.contains(Tmouse.cursorpos) then
    focus := false;

  if rect.contains(Tmouse.cursorpos) and (event = TGUIMouseEvent.left_down) then
  begin
    focus := true;
    {$IFDEF Win32}SoundMaster.Play('click');{$ENDIF}
  end;
end;


procedure TGUIEdit.KeyEvent(var event: TGUIKeyEvent);
begin
  if (focus = true) and (event.state = TGUIKeyState.down) and TextFilter.Filter(event.key, TTextFilter.Username) then
  begin
   {$IFDEF Win32} SoundMaster.Play('hover');    {$ENDIF}

    if length(text) < maxlen then
      text := text + Keyboard.input;

    if (Keyboard.chars[VK_BACK] = true) then
    begin
      SetLength(text, length(text) - 2);
      Keyboard.chars[VK_BACK] := false;
    end;
  end;
end;

procedure TGUIEdit.Logic;
var
  I: integer;
begin
  rect := self.GetRect;
  field.text := '';

  if (focus and Keyboard.chars[VK_TAB]) then
  begin
    Keyboard.chars[VK_TAB] := false;
    TGUIEngine(engine).TabEvent(TGUIElement(self));
  end;

  if mask then
    for I := 0 to length(text) - 1 do
      field.text := field.text + '*'
  else
    field.text := text;
end;

end.