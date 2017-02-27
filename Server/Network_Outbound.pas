unit Network_Outbound;

// everyone who wishes to send data should aqcuire an outboud TNetwork.

interface

// this shouold be a class, loaded by the "main" thread and every map-thread.
uses Classes, IdTCPClient, IdSocketHandle, SysUtils, System_log,
  Network_Inbound, Network_Vars, SyncObjs;

type
  TNetwork = class
  public
    procedure sendpacket(id: string; packtype: TPackType; action: TAction;
      broadcast: boolean = false);
    constructor create;
  private
    transmission: TIdTCPClient;
  end;

procedure initialize();

var
  networking: TNetwork;
  cs: TCriticalSection;

implementation

procedure lock();
begin
  cs.Acquire;
end;

procedure unlock();
begin
  cs.Release;
end;

constructor TNetwork.create;
begin
  transmission := TIdTCPClient.create();
end;



procedure initialize;
begin
  print('Opening outbound Pipe..', Brown);
  networking := TNetwork.create;
  networking.transmission := TIdTCPClient.create();
  cs := TCriticalSection.Create;
  print(#9 + ' Done.', LightGreen, false);
  // tickthread.Execute;
end;

end.
