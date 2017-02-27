unit System_Respawn;

// controls respawning of players in a World.
// if a player is in this list, he is dead.

interface

uses Engine_Characters, SyncObjs;

type
  TPlayerList = Array of ^TCharacter;

type
  TSpawnData = record
    player: ^TCharacter;
    ttl: integer;
  end;

type
  TPlayerRespawn = class
  private
    cs: TCriticalSection;
    respawns: array of TSpawnData;
  public
    constructor create;
    procedure Process();
    procedure Add(var char: TCharacter);
  end;

implementation

constructor TPlayerRespawn.create;
begin
  cs := TCriticalSection.Create;
  SetLength(respawns, 0);
end;

procedure TPlayerRespawn.Add(var char: TCharacter);
begin
  cs.Acquire;
  SetLength(respawns, length(respawns) + 1);
  cs.Release;
end;

procedure TPlayerRespawn.process();
var
  i: Integer;
begin
  cs.acquire;
  for i := 0 to high(respawns) do
    if respawns[i].ttl < 1 then begin
      // remove and send update.

    end;

  cs.release;
end;

end.
