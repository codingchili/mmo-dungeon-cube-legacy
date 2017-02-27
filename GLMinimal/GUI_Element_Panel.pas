unit GUI_Element_Panel;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

uses {$IFDEF Win32} Windows, ShellApi, {$ENDIF}Rectarea,Types, Controls, GUI_Engine, GUI_Element, Vectors2, AsphyreTypes, AbstractCanvas, SysUtils, mouse_proxy,
System_Keyboard;

const
  FRAME_ALPHA = $CC000002;
  FRAME_BORDER = $4400B6A9; // $FF000024;

type
  TGUIPanel = class(TGUIElement)
  private
    dragging: Boolean;
    dragpoint: TPoint;
    alpha: cardinal;
  public
    draggable: Boolean;
    procedure Hide(); override;
    procedure Draw(); override;
    procedure Logic(); override;
    procedure MouseEvent(var event: TGUIMouseEvent); override;
    constructor Create(engine: Pointer; left, top, width, height: integer);
  end;

implementation

uses System_Initializer, Classes, Main;

{ TGUIText }

constructor TGUIPanel.Create(engine: Pointer; left, top, width, height: integer);
begin
  inherited Create(engine, left, top, width, height);
  self.mode := Mode_Pane;
  TGUIEngine(engine).Add(TGUIElement(self));
  self.rect := self.GetRect;
end;

procedure TGUIPanel.Hide;
begin
  visible := false;

  if (dragging) then
  begin
    draglock.unlock;
    dragging := false;
  end;
end;

procedure TGUIPanel.Draw;
var
rect: Rectarea.TRect;
begin
  rect := self.GetRect;
  GameCanvas.FillRect(Classes.rect(rect.left, rect.top, rect.left + rect.width, rect.top + rect.height), FRAME_ALPHA, beNormal);

  GameCanvas.FrameRect(Classes.rect(rect.left - 1, rect.top - 1, rect.left + rect.width + 1, rect.top + rect.height + 1),
    cColor4(FRAME_BORDER));
end;

procedure TGUIPanel.MouseEvent(var event: TGUIMouseEvent);
begin
  if (draggable) and (GetRect.Contains(TMouse.CursorPos)) and (event = TGUIMouseEvent.left_down) then
    if (draglock.trylock) then
    begin
      dragging := true;
      dragpoint.x := (rect.TopLeft.x - TMouse.CursorPos.x);
      dragpoint.y := (rect.TopLeft.Y - TMouse.CursorPos.Y);
    end;

  if (self.dragging) and (event = TGUIMouseEvent.left_up) then
  begin
    draglock.unlock;
    dragging := false;
  end;
end;

procedure TGUIPanel.Logic;
var
  mousepos: TPoint;
begin

  mousepos := TMouse.CursorPos;


  if dragging then
  begin
    rect.left := mousepos.X + dragpoint.X;
    rect.top := mousepos.Y + dragpoint.Y;
    rect.Right := rect.left + rect.width;
    rect.Bottom := rect.top + rect.height;
  end;
end;

end.