unit GUI_Element_Text;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

uses Rectarea, Controls, GUI_Engine, GUI_Element, Vectors2, AsphyreTypes, Classes, Mouse_Proxy;

type
  TGUIText = class(TGUIElement)
  private
    rect: Rectarea.TRect;
    output: string[80];
    drcolor: cardinal;
  public
    text: string[80];
    hvcolor, color: cardinal;
    procedure Draw(); override;
    procedure Logic(); override;
    procedure MouseEvent(var event: TGUIMouseEvent); override;
    constructor Create(engine: Pointer; left, top: integer; text: string; color: cardinal = TEXT_COLOR;
      hovercolor: cardinal = TEXT_COLOR);
  end;

implementation

uses System_Initializer;

{ TGUIText }

constructor TGUIText.Create(engine: Pointer; left, top: integer; text: string; color: cardinal = TEXT_COLOR;
  hovercolor: cardinal = TEXT_COLOR);
begin
  inherited Create(engine, left, top, trunc(GameFonts[0].TextWidth(text)), TEXT_HEIGHT);
  self.mode := Mode_Text; // engine identifier
  self.output := text;
  self.text := text;
  self.color := color;
  self.hvcolor := hovercolor;
  self.drcolor := color;

  TGUIEngine(engine).Add(TGUIElement(self));
end;

procedure TGUIText.MouseEvent(var event: TGUIMouseEvent);
begin
  //
end;

procedure TGUIText.Draw;
begin
  rect := self.GetRect;
  GameFonts.Items[0].TextOut(point2(rect.left, rect.top), text, cColor2(drcolor));
end;

procedure TGUIText.Logic;
begin
  rect := self.GetRect;
  rect.width := trunc(GameFonts[0].TextWidth(text));

  if rect.contains(Tmouse.cursorpos) then
    drcolor := hvcolor
  else
    drcolor := color;
end;

end.