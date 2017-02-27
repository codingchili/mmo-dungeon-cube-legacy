unit System_Ping;

// disconnect/warn users with high ping
{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses
  Classes, IdBaseComponent, IdComponent, IdRawBase, IdRawClient, IdIcmpClient,
  SysUtils, {$IFDEF Win32}Vcl.forms,{$ENDIF} SyncObjs, Debug_Stopwatch;

CONST
  PING_COOLDOWN = 1000;

type
  TPingData = class
  private
    cs: TCriticalSection;
    ms: integer;
    strms: string;
  public
    sw: TStopWatch;
    function PingTime(): integer;
    function PingTimeStr(): string;
    procedure PingQuery();
    procedure PingReply();
    constructor create;
  end;

type
  TPingReply = procedure(Latency: Cardinal);
  TPingThread = class(TThread)
  public
    procedure Execute; override;
  private
    FHost: string;
    FTimeout: integer;
    FOnReply: TPingReply;
    FIdCmp: TIdIcmpClient;
  protected
    procedure OnPingReply(ASender: TComponent;
      const AReplyStatus: TReplyStatus);
  public
    property Host: string read FHost write FHost;
    property Timeout: integer read FTimeout write FTimeout;
    property OnReply: TPingReply read FOnReply write FOnReply;
    destructor Destroy; override;
  end;

procedure Initialize;

var
  RoundTripMs: integer;
  PingThread: TPingThread;
  enabledping: boolean;
  UPing, TPing: TPingData;
  RoundTripMsStr: string[10];

implementation

uses System_Initializer, {$IFDEF Win32}Windows,{$ENDIF} Dialogs, Conf_Protocol, System_Log,
  Network_World, Network_WorldServer;

function TPingData.PingTimeStr: string;
begin
  cs.Acquire;
  result := strms;
  cs.Release;
end;

procedure TPingData.PingQuery;
begin
  sw.Start;
end;

procedure TPingData.PingReply;
begin
  sw.Stop;
  cs.Acquire;
  ms := sw.ElapsedTicks;
  strms := FormatFloat('0.###', ms / 10000);
  cs.Release;
end;

function TPingData.PingTime(): integer;
begin
  cs.Acquire;
  result := ms;
  cs.Release;
end;

constructor TPingData.create;
begin
  cs := TCriticalSection.create;
  sw := TStopWatch.create(false);
end;

destructor TPingThread.Destroy;
begin
  FIdCmp.Free;
  inherited;
end;

procedure TPingThread.Execute;
begin
  FIdCmp := TIdIcmpClient.create(nil);
  // FIdCmp.Host := '192.168.0.12';//conf_protocol.Host;
  FIdCmp.ReceiveTimeout := 499;
  FIdCmp.PacketSize := 28;
  // FIdCmp.Port := 1556;
  FIdCmp.OnReply := OnPingReply;

  while not terminated do
  begin
    try
      FIdCmp.Host := WorldConnection.Host;
      sleep(PING_COOLDOWN);
      WorldConnection.Ping;
      worldcon.Ping;
      FIdCmp.Ping;
    except
      //
    end;
  end;
end;

procedure TPingThread.OnPingReply(ASender: TComponent;
  const AReplyStatus: TReplyStatus);
begin
  Print(Format('%d Byte From %s: icmp_seq=%d ttl=%d Time%s%d ms',
    [AReplyStatus.BytesReceived, AReplyStatus.FromIpAddress,
    AReplyStatus.SequenceId, AReplyStatus.TimeToLive, ' ',
    AReplyStatus.MsRoundTripTime]), System_Log.Brown);

  RoundTripMs := AReplyStatus.MsRoundTripTime;
  RoundTripMsStr := IntToStr(RoundTripMs);
end;

procedure Initialize();
begin
  PingThread := TPingThread.create(false);
  enabledping := true;
end;

end.