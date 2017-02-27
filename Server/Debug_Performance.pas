unit Debug_Performance;

interface

uses SyncObjs, Classes, Sysutils {$IFDEF Win32},TimeSpan, Windows{$ENDIF};

const
  LOGTICKS = TRUE;

type
  TPerformanceLog = class(TThread)
  protected
    procedure Execute; override;
  private
    frequency: int64;
    cs: TCriticalSection;
    fTicks, accTicks: integer;
    function getTicks(): integer;
    function getMs(): single;
  public
    procedure Ticks(amount: integer);
    property TickCount: integer read getTicks;
    property MsCount: single read getMs;
    constructor create();
  end;

procedure initialize;

var
  PerformanceLog: TPerformanceLog;

implementation

uses System_Log;

procedure initialize;
begin
  if (LOGTICKS) then
    PerformanceLog := TPerformanceLog.create;
end;

constructor TPerformanceLog.create;
begin
  inherited create(false);
  cs := TCriticalSection.create;
end;

function TPerformanceLog.getMs(): single;
// var
// frequency: TLargeInteger;
begin
  try
    cs.Acquire;
    try
      result := (fTicks * 100) / { 1000000 } 10000000;
{$IFDEF Windows}
      QueryPerformanceFrequency(frequency);
      result := (fTicks * 1000) / frequency;
      { ELSE }
      result := -1;
{$ENDIF}
    finally
      cs.Release; // server frame dragged, or debug pause
    end;
  except
    //
  end;
end;

function TPerformanceLog.getTicks(): integer;
begin
  cs.Acquire;
  try
    result := fTicks;
  finally
    cs.Release;
  end;
end;

procedure TPerformanceLog.Ticks(amount: integer);
begin
  cs.Acquire;
  try
    accTicks := accTicks + amount;
  finally
    cs.Release;
  end;
end;

procedure TPerformanceLog.Execute;
begin
  while TRUE do
  begin
    sleep(1000);

    cs.Acquire;
    try
      fTicks := accTicks;
      accTicks := 0;
    finally
      cs.Release;
    end;
  end;
end;

end.