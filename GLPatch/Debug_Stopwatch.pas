unit Debug_Stopwatch;

interface

uses {$IFDEF Win32} Windows, {$ENDIF}SysUtils, DateUtils;

type
  TStopWatch = class
  private
    fFrequency, fStartCount, fStopCount: Int64;
    fIsRunning: boolean;
    fIsHighResolution: boolean;
    procedure SetTickStamp(var lInt: int64);
    function GetElapsedTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsed: string;
  public
    constructor Create(const startOnCreate: boolean = false);
    procedure Start;
    procedure Stop;
    property IsHighResolution: boolean read fIsHighResolution;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property Elapsed: string read GetElapsed;
    property IsRunning: boolean read fIsRunning;
  end;


implementation

constructor TStopWatch.Create(const startOnCreate: boolean = false);
begin
  inherited Create;

  fIsRunning := false;

  {$IFDEF Win32}
  fIsHighResolution := QueryPerformanceFrequency(fFrequency);
  {$ENDIF}


  if NOT fIsHighResolution then
    fFrequency := MSecsPerSec;

  if startOnCreate then
    Start;
end;

function TStopWatch.GetElapsedTicks: int64;
begin
  result := fStopCount - fStartCount;
end;

procedure TStopWatch.SetTickStamp(var lInt: int64);
begin
  if fIsHighResolution then
    {$IFDEF Win32}
    QueryPerformanceCounter(lInt)
    {$ENDIF}
  else
    lInt := MilliSecondOf(Now);
end;

function TStopWatch.GetElapsed: string;
var
  dt: TDateTime;
begin
  dt := ElapsedMilliseconds / MSecsPerSec / SecsPerDay;
  result := Format('%d days, %s', [trunc(dt), FormatDateTime('hh:nn:ss.z',
    Frac(dt))]);
end;

function TStopWatch.GetElapsedMilliseconds: int64;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) div fFrequency;
end;

procedure TStopWatch.Start;
begin
  SetTickStamp(fStartCount);
  fIsRunning := true;
end;

procedure TStopWatch.Stop;
begin
  SetTickStamp(fStopCount);
  fIsRunning := false;
end;

begin
end.