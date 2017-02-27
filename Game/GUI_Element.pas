unit GUI_Element;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses Classes, SyncObjs, SysUtils, Rectarea, Effects_Blending;

type
  TGUIModel = (Mode_Background = 0, Mode_Pane = 1, Mode_Image = 2, Mode_Edit = 3, Mode_Button = 4, Mode_Text = 5,
    Mode_TextLines = 6);

type
  TGUIMouseEvent = (left_up, left_down, right_up, right_down, nomouse);

type
  TGUIKeyState = (down, up, nokey);

type
  TGUIKeyEvent = record
  public
    key: word;
    state: TGUIKeyState;
  end;

CONST
  ZERO: integer = 0;
  EMPTY: string = '';

type
  TGUIElement = class
  protected
    parent: Pointer;
  public
    visible: boolean;
    mode: TGUIModel;
    rect: Rectarea.TRect;
    engine: Pointer;
    spritesheet: integer;
    procedure Draw(); virtual;
    procedure Logic(); virtual;
    procedure MouseEvent(var event: TGUIMouseEvent); virtual;
    procedure KeyEvent(var event: TGUIKeyEvent); virtual;
    procedure setparent(ptr: Pointer);
    function GetRect(): TRect; virtual;
    procedure Hide; virtual;
    procedure Show; virtual;
    function GetRootVisible(): boolean;
    constructor create(engine: Pointer; left, top, width, height: integer);
  end;

implementation

uses System_Initializer, GUI_Engine, System_Log;

constructor TGUIElement.create(engine: Pointer; left, top, width, height: integer);
begin
  self.parent := NIL;
  self.engine := engine;
  self.spritesheet := TGUIEngine(engine).spritesheet;
  self.rect := Rectarea.new(left, top, width, height);
  visible := true;
end;

// < non abstract methods allows subclasses to skimp the implementation >
procedure TGUIElement.Draw;
begin
  // < no default handler available, please override >
end;

procedure TGUIElement.Logic;
begin
  // < no default handler available, please override >
end;

procedure TGUIElement.MouseEvent(var event: TGUIMouseEvent);
begin
  // < no default handler available, please override >
end;

procedure TGUIElement.KeyEvent(var event: TGUIKeyEvent);
begin
  // < no default handler available, please override >
end;

procedure TGUIElement.Hide;
begin
  visible := false;
end;

procedure TGUIElement.Show;
begin
  visible := true;
end;

function TGUIElement.GetRect: Rectarea.TRect;
var
  tmp_parent: ^TGUIElement;
begin
  result := Rectarea.new(0, 0, 0, 0);
  tmp_parent := self.parent;

  while (tmp_parent <> NIL) do
  begin
    result.top := result.top + TGUIElement(tmp_parent).rect.top;
    result.left := result.left + TGUIElement(tmp_parent).rect.left;

    tmp_parent := TGUIElement(tmp_parent).parent;
  end;

  result.top := self.rect.top + result.top;
  result.left := self.rect.left + result.left;
  result.width := self.rect.width;
  result.height := self.rect.height;
  result.Right := result.left + rect.width;
  result.Bottom := result.top + rect.height;
end;

function TGUIElement.GetRootVisible: boolean;
var
  ptr: Pointer;
begin
  result := true;
  ptr := self.parent;
  while (ptr <> NIL) and (result = true) do
  begin
    result := TGUIElement(ptr).visible;
    ptr := TGUIElement(ptr).parent;
  end;

end;

procedure TGUIElement.setparent(ptr: Pointer);
begin
  self.parent := ptr;
  self.Logic;
end;

end.
