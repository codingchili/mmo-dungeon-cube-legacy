unit Effects_Lightning;

interface

uses
{$IFDEF Linux}Types, System_Keyboard, {$ENDIF}
{$IFDEF Win32} Windows, Classes, {$ENDIF}
  Math, Controls, SyncObjs, Mouse_Proxy;

const
  UPDATE: integer = 30; // frames between updates

type
  TLightning = Record
    ttl, refresh: integer;
    start, stop: TPoint;
    fire: boolean; // get arctan2 angle here, use cos/sin to find the remaining arcs, seed with random offset angle
    breaks: array [0 .. 4] of TPoint; // breaks in the arc
  end;

type
  TLightningMan = class
  private
    arc: array [1 .. 100] of TLightning;
    cs: TCriticalSection;
  public
    procedure Add(start, stop: TPoint; ttl: integer; fire: boolean = false);
    procedure Draw(); // needs custom draw
    procedure Process;
    constructor create;
  end;

type
  TPlayer = record
    function getX(): integer;
    function getY(): integer;
  end;

type
  TCam = record
    x, y: integer;
  end;

procedure Initialize();

var
  LightningMan: TLightningMan;
  lock: boolean;
  cam: TCam;
  player: TPlayer;

implementation

uses Engine_Particles, System_Initializer, AsphyreTypes, AbstractCanvas, vectors2,
  Effects_Fire;

function TPlayer.getX;
begin
  result := 0;
end;

function TPlayer.getY;
begin
  result := 0;
end;

procedure Initialize();
begin
  LightningMan := TLightningMan.create;
end;

constructor TLightningMan.create;
var
  i: integer;
begin
  cs := TCriticalSection.create;
  for i := low(arc) to high(arc) do
    arc[i].ttl := 0;
end;

procedure TLightningMan.Draw();
var
  i, j: integer;
  lastjoint: TPoint;
begin
  cs.Acquire;
  for i := low(arc) to high(arc) do
    if (arc[i].ttl > 0) then

    begin
      lastjoint := arc[i].start;
      for j := low(arc[i].breaks) to High(arc[i].breaks) do
      begin
        // draw lines
        GameCanvas.Line(Point2(lastjoint.x + cam.x, lastjoint.y + cam.y),
          Point2(arc[i].breaks[j].x + cam.x, arc[i].breaks[j].y + cam.y), $405050CC, $9900FFFF);
        // GameCanvas.Line(Point2(lastjoint.X + cam.X + 1, lastjoint.Y + cam.Y),
        // Point2(arc[i].breaks[j].X + cam.X + 1, arc[i].breaks[j].Y + cam.Y), $FF5050CC, $FF00FFFF);
        // GameCanvas.Line(Point2(lastjoint.X + cam.X + 2, lastjoint.Y + cam.Y),
        // Point2(arc[i].breaks[j].X + cam.X + 2, arc[i].breaks[j].Y + cam.Y), $FF5050CC, $FF00FFFF);
        lastjoint := arc[i].breaks[j];
      end;
    end;
  cs.Release;
end;

procedure TLightningMan.Add(start, stop: TPoint; ttl: integer; fire: boolean = false);
var
  i, j, k: integer;
  dir, range: single;
  dist: integer;
  a2, b2: Extended;
begin
  cs.Acquire;
  for i := low(arc) to High(arc) do
    if (arc[i].ttl = 0) then
    begin
      arc[i].stop := stop;
      arc[i].start := start;
      arc[i].ttl := ttl;
      arc[i].refresh := 10;
      arc[i].fire := fire;
      // calculate arcs here, recalculate in process

      dir := arctan2(start.y - stop.y, start.x - stop.x);
      a2 := abs(stop.x - start.x);
      b2 := abs(stop.y - start.y);
      range := abs(sqrt(Math.Power(a2, 2) + Math.Power(b2, 2)));
      dist := 0;

      for j := low(arc[i].breaks) to High(arc[i].breaks) do
      begin
        dist := dist + round(random(trunc(range / 3)));

        arc[i].breaks[j].x := round(cos(dir + ((6.28 / 360) * random(30) - 60)) * dist + start.x);
        arc[i].breaks[j].y := round(sin(dir + ((6.26 / 360) * random(30) - 60)) * dist + start.y);

        for k := 0 to 3 do
          ParticleMan.fire(arc[i].breaks[j].x, arc[i].breaks[j].y, 1.15, (6.28 / 360) * (90 * k) { random(360) } , 14,
            TParticleEffect.peLightning, $FF00FFFF, 1);
      end;

      // remove at build time, or leave xD
      if (fire) then
        FireMan.Add(arc[i].breaks[high(arc[i].breaks)].x, arc[i].breaks[high(arc[i].breaks)].y, 110);

      // add particle effects at end of arc
      break;
    end;

  // calculate arcs here, recalculate in process
  cs.Release;
end;

// point dir towards triangle top, get dir with atan2  y - y2, x - x2
procedure TLightningMan.Process();
var
  dir: single;
  offset: integer;
  color: cardinal;
  i, j, dist, range: integer;
begin
  if (getasynckeystate(ord('L')) <> 0) and not(lock) then
  begin
    Add(Point(player.getX, player.getY), Point(trunc(Tmouse.CursorPos.x - cam.x), trunc(Tmouse.CursorPos.y - cam.y)),
      35, false);
    Add(Point(player.getX, player.getY), Point(trunc(Tmouse.CursorPos.x - cam.x), trunc(Tmouse.CursorPos.y - cam.y)),
      35, false);
    Add(Point(player.getX, player.getY), Point(trunc(Tmouse.CursorPos.x - cam.x), trunc(Tmouse.CursorPos.y - cam.y)),
      35, false);
    Add(Point(player.getX, player.getY), Point(trunc(Tmouse.CursorPos.x - cam.x), trunc(Tmouse.CursorPos.y - cam.y)),
      35, false);
    // Add(Point(player.getX, player.getY), Point(trunc(Tmouse.CursorPos.X - cam.X), trunc(Tmouse.CursorPos.Y - cam.Y)),
    // 35, true);
    lock := true;
  end;

  if (getasynckeystate(ord('L')) = 0) then
    lock := false;

  cs.Acquire;
  for i := low(arc) to High(arc) do
  begin
    if (arc[i].ttl > 0) then
    begin
      dec(arc[i].ttl);

      // lazy update ^^
      if ((arc[i].ttl mod arc[i].refresh) = 0) then
      begin
        cs.Release;
        Add(arc[i].start, arc[i].stop, arc[i].ttl, arc[i].fire);
        cs.Acquire;
        arc[i].ttl := 0;
      end;
    end;

  end;

  cs.Release;
end;

end.
