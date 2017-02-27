unit System_Keyboard;

interface

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

// This unit controls keyboard and mouse events.

uses
{$IFDEF Win32}Windows,{$ENDIF}
{$IFDEF Linux}LCLIntf, Keyboard, {$ENDIF}

Forms, Controls,  Dialogs, SysUtils,
  Math, Conf_Protocol, System_Initializer,  Mouse_Proxy;

const
  KEYBOARD_PRESS: integer = -32767;
  KEYBOARD_DOWN: integer = -32768;
  KEYBOARD_NONE: integer = 0;

  {$IFDEF Linux}
  VK_BACK = 8;
  VK_TAB = 9;
  VK_RETURN = 13;
  VK_RBUTTON = 2;
  VK_LBUTTON = 1;
  VK_SPACE = 32;
  VK_ESCAPE = 27;
  VK_F5 = $74;
  VK_F2 = $71;
  {$ENDIF}

type
  TKeyboard = class
  public
    chars: Array [0 .. 254] of boolean;
    skills: Array [0 .. 8] of integer;
    input: string;
    lastdir: TDirection;
    inputlock: boolean;
    constructor create();
    procedure clear();
    procedure kbUp(key: Word);
    procedure kbDown(key: Word);
    procedure Process();
    function GetCharFromVKey(vkey: Word): string;
  end;

procedure Initialize();

{$IFDEF Linux}
  function GetAsyncKeyState(key: WORD): int64;
{$ENDIF}

var
  Keyboard: TKeyboard;

implementation

uses Main, {$IFDEF Win32}System_Audio,{$ENDIF} System_Log;

{$IFDEF Linux}
function GetAsyncKeyState(key: WORD): int64;
begin
  result := GetKeyState(key);
end;
{$ENDIF}

constructor TKeyboard.create;
var
  i: integer;
begin
  for i := 0 to high(chars) do
    chars[i] := false;

  skills[0] := VK_RBUTTON;
  skills[1] := ord('1');
  skills[2] := ord('2');
  skills[3] := ord('3');
  skills[4] := ord('4');
  skills[5] := ord('R');
  skills[6] := ord('Q');
  skills[7] := ord('E');

  inputlock := false;
end;

procedure Initialize();
begin
  Keyboard := TKeyboard.create;
End;

// place custom keys here.
procedure TKeyboard.Process();
begin
  if (GetASyncKeystate(vk_tab) = KEYBOARD_PRESS) then
    Keyboard.chars[vk_tab] := true
  else
    Keyboard.chars[vk_tab] := false;
end;

// called after process frame.
procedure TKeyboard.clear();
var
  i: integer;
begin
  for i := 0 to high(chars) do
    chars[i] := false;
end;

procedure TKeyboard.kbUp(key: Word);
begin

end;

// 1-2 ft faster. gShE
procedure TKeyboard.kbDown(key: Word);
var
  i: integer;
  x, y, x_end, y_end, angle: single;
begin
  // exception from input block...
  if key = VK_BACK then
    chars[key] := true;

  if (inputlock = true) then
  begin
    // chars[key] := false;
    exit;
  end;
end;

{$IFDEF Win32}
function TKeyboard.GetCharFromVKey(vkey: Word): string;
var
  keystate: TKeyboardState;
  retcode: integer;
begin
  Win32Check(GetKeyboardState(keystate));
  SetLength(Result, 2);
  retcode := ToAscii(vkey, MapVirtualKey(vkey, 0), keystate, @Result[1], 0);
  case retcode of
    0:
      Result := '';
    1:
      SetLength(Result, 1);
    2:
      ;
  else
    Result := '';
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
function TKeyboard.GetCharFromVKey(vkey: Word): string;
begin
 result := Chr(vkey);
end;
{$ENDIF}

begin

END.