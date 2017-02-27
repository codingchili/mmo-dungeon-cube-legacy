unit GUI_Engine;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}
// Supported controls: background-image, Buttons, Text Fields, Text, Frames
// Supported design functions: dragging frames, multi-level parenting of controls, focus system

uses SysUtils, {$IFDEF Win32}Winapi.Windows, {$ENDIF} Classes, AsphyreTypes, Controls, SyncObjs, System_Keyboard,
  GUI_Element;

CONST
  TEXT_COLOR = $FFFFFFFF;
  TEXT_HEIGHT = 12;

type
  TGUIEngine = class
  public
    cs: TCriticalSection;
    spritesheet: integer;
    visible: boolean;
    constructor create(spritesheet: string);
    procedure MouseEvent(event: TGUIMouseEvent);
    procedure KeyEvent(event: TGUIKeyEvent);
    procedure Add(var element: TGUIElement);
    procedure sort(iLo, iHi: integer);
    procedure Logic();
    procedure Draw();
    procedure Clear;
    procedure TabEvent(var element: TGUIElement);
    procedure pack();
  private
    element: array of Pointer;
  end;

type
  TDragLock = class
  private
    locked: boolean;
    cs: TCriticalSection;
  public
    function trylock(): boolean;
    procedure unlock();
    constructor create();
  end;

procedure Initialize;

// only drag one frame at a time.
var
  DragLock: TDragLock;

implementation

uses System_Initializer, Main, System_Log, Debug_StopWatch, GUI_Element_Edit,
  GUI_Element_Text{$IFDEF Win32}, System_Audio{$ENDIF};

constructor TDragLock.create();
begin
  locked := false;
  cs := TCriticalSection.create;
end;

// create the gui engine.
constructor TGUIEngine.create(spritesheet: string);
begin
  SetLength(element, 0);
  self.spritesheet := GameImages.Resolve(spritesheet);
  cs := TCriticalSection.create;
end;

// set focus to next visible object of type (element)
procedure TGUIEngine.TabEvent(var element: TGUIElement);
var
  i, firstpos: integer;
  first, seek, found: boolean;
begin
  first := false;
  seek := false;
  found := false;

  for i := 0 to high(self.element) do
  begin

    if (TGUIElement(self.element[i]).mode = element.mode) and (TGUIElement(self.element[i]).visible) and
      (element.visible) and not(first) then
    begin
      firstpos := i;
      first := true;
    end;

    if (integer(element) = integer(self.element[i])) then
    begin
      seek := true;
      continue;
    end;

    if (seek) then
      if (TGUIElement(self.element[i]).mode = element.mode) and (TGUIElement(self.element[i]).visible = true) and
        (TGUIElement(self.element[i]).GetRootVisible) then
      begin
        TGUIEdit(self.element[i]).focus := true;
        TGUIEdit(element).focus := false;
        found := true;
        break;
      end;
  end;
  if not(found) and (first) then
  begin
    TGUIEdit(self.element[firstpos]).focus := true;
    TGUIEdit(element).focus := false;
  end;

{$IFDEF Win32}
  SoundMaster.Play('click');
{$ENDIF}
end;

// returns true if lock succeeded.
function TDragLock.trylock;
begin
  cs.Acquire;
  result := not locked;

  if (not locked) then
    locked := true;
  cs.Release;
end;

procedure TDragLock.unlock;
begin
  cs.Acquire;
  locked := false;
  cs.Release;
end;

procedure TGUIEngine.Logic;
var
  i, len: integer;
begin
  cs.Acquire;
  len := length(self.element);
  for i := 0 to len - 1 do
    if (TGUIElement(element[i]).visible) and (TGUIElement(element[i]).GetRootVisible) then
      TGUIElement(element[i]).Logic();
  cs.Release;
end;

procedure TGUIEngine.KeyEvent(event: TGUIKeyEvent);
var
  i: integer;
begin
  for i := 0 to high(element) do
    if (TGUIElement(element[i]).visible) and (TGUIElement(element[i]).GetRootVisible) then
      TGUIElement(element[i]).KeyEvent(event);
end;

procedure TGUIEngine.MouseEvent(event: TGUIMouseEvent);
var
  i: integer;
begin
  for i := high(element) downto 0 do
    if (TGUIElement(element[i]).visible) and (TGUIElement(element[i]).GetRootVisible) then
      TGUIElement(element[i]).MouseEvent(event);
end;

// draw all items.
procedure TGUIEngine.Draw();
var
  i, len: integer;
begin
  cs.Acquire;
  len := length(self.element);
  for i := 0 to len - 1 do
    if (TGUIElement(element[i]).visible) and (TGUIElement(element[i]).GetRootVisible) then
      TGUIElement(element[i]).Draw;
  cs.Release;
end;

// clear all gui items.
procedure TGUIEngine.Clear;
var
  i: integer;
begin
  for i := 0 to high(element) do
    TGUIElement(element[i]^).Free;

  SetLength(element, 0);
end;

procedure TGUIEngine.sort(iLo, iHi: integer);
var
  Lo, Hi, Pivot: integer;
  T: ^TGUIElement;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := ord(TGUIElement(element[(Lo + Hi) div 2]).mode);
  repeat
    while ord(TGUIElement(element[Lo]).mode) < Pivot do
      Inc(Lo);
    while ord(TGUIElement(element[Hi]).mode) > Pivot do
      Dec(Hi);
    if Lo <= Hi then
    begin
      T := element[Lo];
      element[Lo] := element[Hi];
      element[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then
    sort(iLo, Hi);
  if Lo < iHi then
    sort(Lo, iHi);
end;

procedure TGUIEngine.pack;
begin
  self.sort(low(self.element), high(self.element));
end;

// add object to engine
procedure TGUIEngine.Add(var element: TGUIElement);
var
  len: integer;
begin
  try
    len := length(self.element);
    SetLength(self.element, len + 1);
    self.element[len] := Pointer(element);
  except
    on E: Exception do
      print(E.Message);
  end;
end;

procedure Initialize;
begin
  DragLock := TDragLock.create;
end;

end.
