unit System_Mouse;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

// keeps a reference to every engine? engines must Mouse.RegisterInput(self) at startup.
// loosely coupled with GUI_Engine

uses SysUtils, Classes, {$IFDEF Win32}Windows,{$ENDIF} GUI_Element, Controls, Messages, Forms
  {$IFDEF Linux}, System_Keyboard{$ENDIF};

const
  MOUSE_PRESS: integer = -32767;
  MOUSE_DOWN: integer = -32768;
  MOUSE_NONE: integer = 0;

type
  TEvent = (MouseUp, MouseDown, MouseNone);

type
  TButton = (ScrollUp, ScrollDown, Left, Right, None);

type
  TEventType = (Button, Scroll);

type
  TScrollDirection = (Up, Down);

type
  TMouseState = record
    event: TEvent;
    Button: TButton;
  end;

type
  TGUIRegisteredEvent = record
  public
    element: TGUIElement;
    eventype: TEventType;
  end;

type
  TMouse = class(TThread)
  protected
    procedure Execute; override;
  private
    Engines: array of TGUIRegisteredEvent;
  public
    procedure NotifyScroll(direction: TScrollDirection);
    procedure RegisterScrollEvent(element: TGUIElement);
    procedure RegisterClickEvent(element: TGUIElement);
    constructor create;
  end;

procedure Initialize;

var
  MouseInput: TMouse;

implementation

uses System_Log, Main;

constructor TMouse.create();
begin
  inherited create(false);
  setlength(Engines, 0);
end;

procedure TMouse.Execute;
begin
  while true do
  begin
    sleep(1);
    if (GetAsyncKeyState(VK_LBUTTON) = MOUSE_PRESS) then
      print('Mouse Click Event Received!');
  end;
end;

procedure TMouse.NotifyScroll(direction: TScrollDirection);
begin

end;

procedure TMouse.RegisterScrollEvent(element: TGUIElement);
begin
end;

procedure TMouse.RegisterClickEvent(element: TGUIElement);
begin

end;

procedure Initialize();
begin
  //MouseInput := TMouse.create;
end;

end.