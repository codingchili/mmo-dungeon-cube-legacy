unit System_DayTime;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses {$IFDEF Win32}Windows, {$ENDIF} SysUtils, SyncObjs, Classes;

const
  CLOCK_AMPLIFIER = 1; // 1s IRL = 60s in-game. // needs to account for TPS vs client SPD & ClockAmp

  { PRE_ZENITH = 11;// sun starts to approach zenith.
    ZENITH = 12;     // sun at its zenith
    NOON = 13;       // sun leaving zenith
    AFTERNOON = 15;  // zenith left
    SUNSET = 19;     // night begins
    DUSK = 21;       // fully dark
    EVENING = 22;    // -//-
    MIDNIGHT = 0;    // -//-
    DAWN = 4;        // sun rises
    SUNRISE = 6;     // sun has arose
    MORNING = 8;     // morning 8-12, time spec only. }

type
  TDayCycle = (PRE_ZENITH = 11, ZENITH = 12, NOON = 13, AFTERNOON = 15, SUNSET = 19, DUSK = 21, EVENING = 22,
    MIDNIGHT = 0, DAWN = 4, SUNRISE = 6, MORNING = 8);

type
  TDayTime = class(TThread)
  protected
    procedure Execute; Override;
  private
    cs: TCriticalSection;
    time, tick: integer; // specify hours for day, night, noon and dusk. receive time from server at login.
    function hour(hours: integer): integer;
  public
    function Cycle(): TDayCycle;
    function FormatTime(): String;
    function ToSec(): integer;
    procedure Secs(time: integer);
    constructor create();

    class function RealTime(): String;
  end;

procedure Initialize;

var
  DayTime: TDayTime;

implementation

uses Engine_World, System_Log, Conf_Server;

procedure Initialize;
begin
  print('Starting Time..  ' + #9);
  DayTime := TDayTime.create;
  print(' ' + DayTime.FormatTime, LightGreen, false);
end;

class function TDayTime.RealTime(): String;
begin
  result := TimeToStr(now);
end;

function TDayTime.hour(hours: integer): integer;
begin
  result := hours * 3600;
end;

function TDayTime.ToSec: integer;
begin
  cs.Acquire;
  result := time;
  cs.Release;
end;

constructor TDayTime.create;
var
  hrs, min, sec, ms: Word;
begin
  inherited create(false);
  cs := TCriticalSection.create;
  DecodeTime(now(), hrs, min, sec, ms);
  time := hour(hrs) + min * 60 + sec;
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

{ PRE_ZENITH = 11, ZENITH = 12, NOON = 13, AFTERNOON = 15, SUNSET = 19, DUSK = 21, EVENING = 22,
  MIDNIGHT = 0, DAWN = 4, SUNRISE = 6, MORNING = 8 }
// get the day cycle, for day/night spells, quests, npcs events etc!
function TDayTime.Cycle(): TDayCycle;
begin
  cs.Acquire;
  case (time DIV 3600) of
    ord(PRE_ZENITH) .. ord(ZENITH) - 1:
      result := TDayCycle.PRE_ZENITH;
    ord(ZENITH) .. ord(NOON) - 1:
      result := TDayCycle.ZENITH;
    ord(NOON) .. ord(AFTERNOON) - 1:
      result := TDayCycle.NOON;
    ord(AFTERNOON) .. ord(SUNSET) - 1:
      result := TDayCycle.AFTERNOON;
    ord(SUNSET) .. ord(DUSK) - 1:
      result := TDayCycle.SUNSET;
    ord(DUSK) .. ord(EVENING) - 1:
      result := TDayCycle.DUSK;
    ord(EVENING) .. 24:
      result := TDayCycle.EVENING;
    ord(MIDNIGHT) .. ord(DAWN) - 1:
      result := TDayCycle.MIDNIGHT;
    ord(DAWN) .. ord(SUNRISE) - 1:
      result := TDayCycle.DAWN;
    ord(SUNRISE) .. ord(MORNING) - 1:
      result := TDayCycle.SUNRISE;
    ord(MORNING) .. ord(PRE_ZENITH) - 1:
      result := TDayCycle.MORNING;
  end;
  cs.Release;
end;

procedure TDayTime.Execute;
begin
  while true do
  begin
    sleep(1000 div TPS);
    cs.Acquire;
    inc(tick);
    time := time + CLOCK_AMPLIFIER;

    if (time > 86399) then
      time := 0;
    cs.Release;
  end;
end;

procedure TDayTime.Secs(time: integer);
begin
  // set color based on time, and set the clock to time
  cs.Acquire;
  self.time := time;
  cs.Release;
end;

end.