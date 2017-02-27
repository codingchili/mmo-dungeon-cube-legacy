unit Effects_Fire;

interface

uses Math, {$IFDEF Win32}Windows,{$ENDIF} Controls, SyncObjs, Mouse_Proxy
  {$IFDEF Linux} ,System_Keyboard{$ENDIF}; // remove windows and controls

  type
  Tcam = record
    x, y: integer;
  end;

type
  TFire = Record
    x, y, ttl: integer;
  end;

type
  TFireMan = class
  private
    fires: array [1 .. 100] of TFire;
    cs: TCriticalSection;
  public
    procedure Add(x, y, ttl: integer);
    procedure Process;
    constructor create;
  end;

procedure INitialize();

var
  Fireman: TFireMan;
  lock: boolean;
  cam: TCam;

implementation

uses Engine_Particles;

procedure INitialize();
begin
  Fireman := TFireMan.create;
end;

constructor TFireMan.create;
var
  i: integer;
begin
  cs := TCriticalSection.create;
  for i := low(fires) to high(fires) do
    fires[i].ttl := 0;
end;

procedure TFireMan.Add(x: integer; y: integer; ttl: integer);
var
  i: integer;
begin
  cs.Acquire;
  for i := low(fires) to High(fires) do
    if (fires[i].ttl = 0) then
    begin
      fires[i].x := x;
      fires[i].y := y;
      fires[i].ttl := ttl;
      break;
    end;
  cs.Release;
end;

// point dir towards triangle top, get dir with atan2  y - y2, x - x2
procedure TFireMan.Process();
var
  dir: single;
  offset: integer;
  color: cardinal;
  i: integer;
begin
  if (getasynckeystate(ord('G')) <> 0) and not(lock) then
  begin
    Add(abs(trunc(Tmouse.CursorPos.x + abs(cam.x))), (trunc(Tmouse.CursorPos.y + abs(cam.y))), 600);
    lock := true;
  end;

  if (getasynckeystate(ord('G')) = 0) then
    lock := false;

  cs.Acquire;
  for i := low(fires) to High(fires) do
  begin
    if fires[i].ttl > 0 then
    begin
      dec(fires[i].ttl);

      offset := random(30) - 15;
      dir := arctan2(22, offset);

      if (offset < 5) and (offset > -5) then
      begin
        color := $FF242424;
        ParticleMan.Fire(fires[i].x + offset, fires[i].y, -0.20, dir, 12 + random(6), TParticleEffect.peFumeify, color,
          random(4) + 12);

      end
      else
      begin
        color := $FFFF6464;
        ParticleMan.Fire(fires[i].x + offset, fires[i].y, -0.15 * (random(1) + 1), dir, 26 + random(20),
          TParticleEffect.peHotcold, color, random(2) + 8);
      end;
    end;
  end;
  cs.Release;
end;

end.