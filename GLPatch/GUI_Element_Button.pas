unit GUI_Element_Button;

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses
{$IFDEF Win32} Windows, {$ENDIF}
  GUI_Engine, GUI_Element, GUI_Element_Text, Controls, Vectors2,
  AsphyreTypes, AbstractCanvas, SysUtils, Rectarea, Types, System_Keyboard, Mouse_Proxy;

const
  BUTTON_WIDTH: integer = 128;
  BUTTON_HEIGHT: integer = 21;

type
  TOnCLick = procedure of object;

type
  TGUIButton = class(TGUIElement)
  protected
  private
    // text, output: string;
    default: boolean;
    caption: TGUIText;
    filter: cardinal;
  public
    enabled, hover: boolean;
    color: cardinal;
    sprite, fsprite: TPoint;
    OnClick: TOnCLick;
    procedure filterwith(filter: cardinal);
    procedure Draw(); override;
    procedure Logic(); override;
    procedure SetDefault;
    procedure SetCaption(text: string);

    procedure MouseEvent(var event: TGUIMouseEvent); override;
    procedure KeyEvent(var event: TGUIKeyEvent); override;

    constructor Create(engine: Pointer; left, top: integer; sprite, fsprite: TPoint; text: string;
      OnClick: TOnCLick); overload;
    constructor Create(engine: Pointer; left, top, width, height: integer; sprite, fsprite: TPoint; text: string;
      OnClick: TOnCLick); overload;
  end;

implementation

uses System_Initializer, System_Log {$IFDEF Win32}, System_Audio{$ENDIF};

{ TGUIText }

procedure TGUIButton.SetDefault;
begin
  self.default := true;
end;

procedure TGUIButton.filterwith(filter: cardinal);
begin
  self.filter := filter;
end;

constructor TGUIButton.Create(engine: Pointer; left, top, width, height: integer; sprite, fsprite: TPoint; text: string;
  OnClick: TOnCLick);
begin
  inherited Create(engine, left, top, width, height);
  self.mode := Mode_Button;
  self.OnClick := OnClick;
  self.sprite := sprite;
  self.fsprite := fsprite;
  self.enabled := true;
  self.default := false;
  self.color := $FFFFFFFF;

  caption := TGUIText.Create(engine, trunc((rect.width / 2) - (GameFonts.Items[0].TextWidth(text) / 2)), 3, text,
    TEXT_COLOR);
  caption.setparent(self);

  TGUIEngine(engine).Add(TGUIElement(self));
  caption.Logic;
end;

constructor TGUIButton.Create(engine: Pointer; left, top: integer; sprite, fsprite: TPoint; text: string;
  OnClick: TOnCLick);
begin
  inherited Create(engine, left, top, BUTTON_WIDTH, BUTTON_HEIGHT);
  self.mode := Mode_Button;
  self.OnClick := OnClick;
  self.sprite := sprite;
  self.fsprite := fsprite;
  self.enabled := true;
  self.color := $FFFFFFFF;
  self.default := false;

  caption := TGUIText.Create(engine, trunc((rect.width / 2) - (GameFonts.Items[0].TextWidth(text) / 2)), 3, text,
    TEXT_COLOR);
  caption.setparent(self);

  TGUIEngine(engine).Add(TGUIElement(self));
  caption.Logic;
end;

procedure TGUIButton.SetCaption(text: string);
begin
  caption.text := text;
  caption.rect.left := round((rect.width / 2) - (GameFonts.Items[0].TextWidth(text) / 2));
end;

procedure TGUIButton.MouseEvent(var event: TGUIMouseEvent);
begin
  if (self.GetRect.Contains(TMouse.CursorPos)) and (event = TGUIMouseEvent.left_down) and (enabled) and (@OnClick <> NIL)
  then
  begin
{$IFDEF Win32}SoundMaster.Play('click'); {$ENDIF}
    self.OnClick();
    event := TGUIMouseEvent.nomouse;
  end;
end;

procedure TGUIButton.KeyEvent(var event: TGUIKeyEvent);
begin
  if (default) and (visible) and (event.key = VK_RETURN) and (event.state = TGUIKeyState.down) then
  begin
    OnClick();
    event.state := TGUIKeyState.nokey;
  end;
end;

procedure TGUIButton.Draw;
var
  rect: Rectarea.TRect;
begin
  rect := self.GetRect;

  if (hover) then
    GameCanvas.UseImagePx(GameImages.Items[self.spritesheet], Point4(fsprite.x, fsprite.y, rect.width + fsprite.x,
      fsprite.y, rect.width + fsprite.x, rect.height + fsprite.y, fsprite.x, rect.height + fsprite.y))
  else
    GameCanvas.UseImagePx(GameImages.Items[self.spritesheet], Point4(sprite.x, sprite.y, rect.width + sprite.x,
      sprite.y, rect.width + sprite.x, rect.height + sprite.y, sprite.x, rect.height + sprite.y));

  GameCanvas.TexMap(pBounds4(rect.left, rect.top, rect.width, rect.height), cColor4(filter), TBlendingEffect.beNormal);
end;

procedure TGUIButton.Logic();
var
  rect: Rectarea.TRect;
begin
  rect := self.GetRect;

  if (rect.Contains(TMouse.CursorPos)) then
  begin
    if (enabled) then
    begin

{$IFDEF Win32} if (not hover) then
        SoundMaster.Play('hover'); {$ENDIF Win32}
      hover := true
    end;
  end
  else
    hover := false;
end;

end.
