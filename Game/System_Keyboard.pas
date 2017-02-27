unit System_Keyboard;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}
// This unit controls keyboard and mouse events.

uses
{$IFDEF Win32}Windows, {$ENDIF}
{$IFDEF Linux}LCLIntf, Keyboard, {$ENDIF}
  Forms, Controls, System_Camera, Dialogs, SysUtils,
  Math, Conf_Protocol, Network_WorldServer, System_Initializer, Engine_Player, Mouse_Proxy;

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
  VK_SHIFT = $10;
  VK_F5 = $74;
  VK_F2 = $71;
  VK_UP = $26;
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
    function GetAsyncKeyState(key: Word): int64;
  end;

procedure Initialize();

var
  Keyboard: TKeyboard;

implementation

uses Main, Engine_Projectiles, {$IFDEF Win32}System_Audio, {$ENDIF} System_Log, System_Spells, Conf_SpritemAp;

function TKeyboard.GetAsyncKeyState(key: Word): int64;
begin
  if (renderform.Focused) then
{$IFDEF Linux}
    result := GetKeyState(key)
{$ENDIF}
{$IFDEF Win32}
      result := Windows.GetAsyncKeyState(key)
{$ENDIF}
  else
    result := 0;
end;

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
  if (Keyboard.GetAsyncKeyState(VK_TAB) = KEYBOARD_PRESS) then
    Keyboard.chars[VK_TAB] := true
  else
    Keyboard.chars[VK_TAB] := false;
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

  if (chars[VK_SPACE]) then
  begin
    player.x := trunc(Tmouse.CursorPos.x - cam.x);
    player.y := trunc(Tmouse.CursorPos.y - cam.y);
  end;

  if (key = ord('H')) then
  begin
    x := player.x - player.size / 2;
    y := player.y - player.size / 2;

    x_end := (Tmouse.CursorPos.x - cam.x);
    y_end := (Tmouse.CursorPos.y - cam.y);

    angle := ArcTan2(y_end - y, x_end - x);

    for i := 0 to 200 do
      ProjectileMan.Fire(trunc(player.x), trunc(player.y), 3.25, angle + (6.28 / 360) * (random(360)), 750,
        SPELL_THIEF_BASIC);
  end;
end;

{$IFDEF Win32}

function TKeyboard.GetCharFromVKey(vkey: Word): string;
var
  keystate: TKeyboardState;
  retcode: integer;
begin
  Win32Check(GetKeyboardState(keystate));
  SetLength(result, 2);
  retcode := ToAscii(vkey, MapVirtualKey(vkey, 0), keystate, @result[1], 0);
  case retcode of
    0:
      result := '';
    1:
      SetLength(result, 1);
    2:
      ;
  else
    result := '';
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