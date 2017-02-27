unit Effects_Water;

interface

uses Math, {$IFDEF Win32}Windows, {$ENDIF} Controls, SyncObjs, Mouse_Proxy;

type
  TWater = Record
    x, y, dirmin, dirmax: integer;
    alive: boolean;
  end;

type
  TWaterMan = class
  private
    water: array [1 .. 20] of TWater;
  public
    procedure Add(x, y, dirmin, dirmax: integer);
    procedure Process;
    constructor create;
  end;

procedure INitialize();

var
  Waterman: TWaterMan;
  lock: boolean;

implementation

uses Engine_Particles, System_Camera, System_initializer;

procedure INitialize();
begin
  Waterman := TWaterMan.create;
end;

constructor TWaterMan.create;
var
  i: integer;
begin
  for i := low(water) to high(water) do
    water[i].alive := false;

  // Add(0, -50, -15, 30);
  // Add(-100, -15, 30, 75);
  // Add(-175, 5, 75, 180);
end;

procedure TWaterMan.Add(x: integer; y: integer; dirmin: integer; dirmax: integer);
var
  i: integer;
begin
  for i := low(water) to High(water) do
    if not(water[i].alive) then
    begin
      water[i].alive := true;
      water[i].x := x;
      water[i].y := y;
      water[i].dirmin := dirmin;
      water[i].dirmax := dirmax;
      break;
    end;
end;

// point dir towards triangle top, get dir with atan2  y - y2, x - x2
procedure TWaterMan.Process();
var
  color, tone: cardinal;
  i, offset: integer;
begin
  for i := low(water) to High(water) do
  begin
    if (water[i].alive) then
    begin
      offset := random(80) - 40;
      color := random($FF);

      if (color > $88) then
      begin
        tone := random($FF) + 88;
        color := color or (tone * $FF) or (tone * $FF * $FF)
      end
      else
      begin
        tone := random(88);
        color := color or (tone * $FF) or (tone * $FF * $FF)
      end;

      color := color or ($44000000);

      ParticleMan.Fire(water[i].x + offset + (Initializer.swidth div 2),
        water[i].y + offset + (Initializer.sheight div 2), 1.5,
        DegToRad(random(water[i].dirmax - water[i].dirmin) + water[i].dirmin), 1400, TParticleEffect.peWater, color,
        random(80) + 85);
    end;
  end;
end;

end.