unit Effects_Smoke;

interface

uses Math, {$IFDEF Win32}Windows, {$ENDIF} Controls, SyncObjs, Mouse_Proxy; // remove windows and controls

type
  TSmoke = Record
    ttl: integer;
    x, y: single;
    px, py: ^single;
    color: cardinal;
    targeted: boolean; // to better follow players on stealth, and also for following a projectile.
  end;

type
  TSmokeMan = class
  private
    smokes: array [1 .. 100] of TSmoke;
    cs: TCriticalSection;
  public
    procedure Add(x, y, ttl: integer; color: cardinal);
    procedure AddTargeted(var x: single; var y: single; ttl: integer; color: cardinal);
    procedure Process;
    constructor create;
  end;

procedure INitialize();

var
  SmokeMan: TSmokeMan;
  lock: boolean;

implementation

uses Engine_Particles, System_Camera, System_Keyboard, Engine_Player;

procedure INitialize();
begin
  SmokeMan := TSmokeMan.create;
end;

constructor TSmokeMan.create;
var
  i: integer;
begin
  cs := TCriticalSection.create;
  for i := low(smokes) to high(smokes) do
    smokes[i].ttl := 0;
end;

// may target players/projectiles
procedure TSmokeMan.AddTargeted(var x: single; var y: single; ttl: integer; color: cardinal);
var
  i: integer;
begin
  cs.Acquire;
  for i := low(smokes) to High(smokes) do
    if (smokes[i].ttl = 0) then
    begin
      smokes[i].x := x;
      smokes[i].y := y;
      smokes[i].ttl := ttl;
      smokes[i].color := color;
      smokes[i].px := @x;
      smokes[i].py := @y;
      smokes[i].targeted := true;

      break;
    end;
  cs.Release;
end;

procedure TSmokeMan.Add(x: integer; y: integer; ttl: integer; color: cardinal);
var
  i: integer;
begin
  cs.Acquire;
  for i := low(smokes) to High(smokes) do
    if (smokes[i].ttl = 0) then
    begin
      smokes[i].x := x;
      smokes[i].y := y;
      smokes[i].ttl := ttl;
      smokes[i].color := color;
      smokes[i].targeted := false;

      break;
    end;
  cs.Release;
end;

// point dir towards triangle top, get dir with atan2  y - y2, x - x2
procedure TSmokeMan.Process();
var
  i: integer;
begin
  if (Keyboard.getasynckeystate(VK_SHIFT) <> 0) and not(lock) then
  begin
    Add(player.getX, player.getY, 10, $FFFFFFFF);
    lock := true;
  end;

  if (Keyboard.getasynckeystate(VK_SHIFT) = 0) and (lock = true) then
  begin
    lock := false;
    Add(player.getX, player.getY, 10, $FFFFFFFF);
  end;

  cs.Acquire;
  try
    for i := low(smokes) to High(smokes) do
    begin
      if smokes[i].ttl > 0 then
      begin
        dec(smokes[i].ttl);

        // if smoke is targeted, but parent does not exist, set ttl to 0
        if (smokes[i].targeted) then
        begin
          smokes[i].x := smokes[i].px^;
          smokes[i].y := smokes[i].py^;
        end;

        ParticleMan.fire(smokes[i].x { smokes[i].x } - 28, smokes[i].y { + smokes[i].y } - 28, 1.08,
          DegToRad(random(360)), 70, TParticleEffect.peSmoke, smokes[i].color, random(40) + 42);
      end;
    end;
  finally
    cs.Release;
  end;
end;

end.