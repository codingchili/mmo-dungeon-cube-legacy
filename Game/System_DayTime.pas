unit System_DayTime;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses {$IFDEF Win32}Windows, {$ENDIF} SysUtils, Math, System_Keyboard, SyncObjs;

const
  DARKEST_NIGHT = $A0;
  BRIGHTEST_DAY = $28;
  CLOCK_AMPLIFIER = 1; // 1s IRL = 60s in-game.

type
  TDayTime = class
  private
    cs: TCriticalSection;
    time, tick: integer; // specify hours for day, night, noon and dusk. receive time from server at login.
    function hour(hours: integer): integer;
  public
    function FormatTime(): String;
    procedure Process();
    procedure Draw();
    procedure Secs(time: integer);
    function PromoteColor(color: cardinal; level: integer): cardinal;
    class function RealTime(): String;
    constructor create();
  end;

procedure Initialize;

var
  DayTime: TDayTime;

implementation

uses AbstractCanvas, GUI_Chat, System_Initializer, System_Log, System_Camera;

procedure Initialize;
begin
  DayTime := TDayTime.create;
end;

class function TDayTime.realTime(): String;
begin
  result := TimeToStr(now);
end;

function TDayTime.hour(hours: integer): integer;
begin
  result := hours * 3600;
end;

constructor TDayTime.create;
var
  hrs, min, sec, ms: Word;
begin
  cs := TCriticalSection.create;
  DecodeTime(now(), hrs, min, sec, ms);
  time := hour(hrs) + min * 60 + sec;
end;

function TDayTime.PromoteColor(color: cardinal; level: integer): cardinal;
var
  i, j: integer;
begin
  for i := 0 to (level * 2) - 1 do
  begin
    for j := 0 to 3 do
      color := color * 2;
  end;

  result := color;
end;

function TDayTime.FormatTime;
var
  clock: integer;
  hours: integer;
  minutes: integer;
  seconds: integer;
begin
  cs.Acquire;
  clock := time;
  cs.Release;

  hours := clock div 3600;
  clock := clock - hours * 3600;

  minutes := clock div 60;
  clock := clock - minutes * 60;

  seconds := clock;
  result := TimeToStr(EncodeTime(hours, minutes, seconds, 0));
end;

procedure TDayTime.Process;
var
  i: integer;
begin

  cs.Acquire;
  inc(tick);
  time := time + CLOCK_AMPLIFIER;

  if (time = (20 * 3600)) then
    ChatEngine.addMessage('', 'It''s getting darker, you should find cover.', SYSTEM_COLOR);

  if (time = (21 * 3600)) then
    ChatEngine.addMessage('', 'Night Falls, Beware of Those Who Lurk The Shadow.', SYSTEM_COLOR);

  if (time = (6 * 3600)) then
    ChatEngine.addMessage('', 'The Sun Rises and Purges the Shadows!', SYSTEM_COLOR);

  if (time > 86399) then
    time := 0;
  cs.Release;
end;

procedure TDayTime.Secs(time: integer);
begin
  // set color based on time, and set the clock to time
  cs.Acquire;
  self.time := time;
  cs.Release;
end;

// todo: if inside (house, cave, dungeon etc..) dont draw SUN,
procedure TDayTime.Draw();
var
  dark, light: cardinal;
  clock: integer; // cache
begin
  cs.Acquire;
  clock := time;
  cs.Release;

  // dawn.
  if (clock < hour(8)) then
  begin
    if (clock > hour(4)) then // offset,  interval, goes from 0..1
      dark := DARKEST_NIGHT - trunc(DARKEST_NIGHT * ((clock - hour(4)) / hour(4)))
    else
      dark := DARKEST_NIGHT;

    dark := PromoteColor(dark, 3);
    GameCanvas.FillRect(0, 0, initializer.swidth, initializer.sheight, dark, TBlendingEffect.beNormal);
  end;

  // dusk.
  if (clock > hour(18)) then
  begin
    if (clock < hour(21)) then // offset, interval goes from 0..1
      dark := trunc(DARKEST_NIGHT * ((clock - hour(18)) / hour(3)))
    else
      dark := DARKEST_NIGHT;

    dark := PromoteColor(dark, 3);
    GameCanvas.FillRect(0, 0, initializer.swidth, initializer.sheight, dark, TBlendingEffect.beNormal);
  end;

  // zenith
  if (clock > hour(10)) and (clock < hour(16)) then
  begin
    // approach zenith
    if (clock > hour(10)) and (clock < hour(12)) then
      light := trunc(BRIGHTEST_DAY * ((clock - hour(10)) / hour(2)))
      // leave zenith
    else if (clock >= hour(14)) and (clock < hour(16)) then
      light := BRIGHTEST_DAY - trunc(BRIGHTEST_DAY * ((clock - hour(14)) / hour(2)))
      // at zenith
    else
      light := BRIGHTEST_DAY;

    light := light or PromoteColor(light, 1);
    light := light or PromoteColor(light, 2);
    GameCanvas.FillRect(0, 0, initializer.swidth, initializer.sheight, light + $40000000, TBlendingEffect.beAdd);
  end;
end;

end.
