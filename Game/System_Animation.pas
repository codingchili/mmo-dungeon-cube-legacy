unit System_Animation;

// attach an animation object to stuff you want to be animated.
// specify the filename?, pattern width, height, and pattern count.
// specify fps, how often the animation should change, specify different
// animation states, depending on the animation state the animation object
// will return different values when drawing.

// create your sprite sheets so that every animation frame has its own x-line.

// this class needs to be easy to use, easy to apply and work with all kind
// of animations. Buildings, characters, spells,

// limitation: animations must be placed on the same x-line.

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

uses SysUtils, Classes, {$IFDEF Win32}Windows,{$ENDIF} AsphyreTypes;

type
  eAnim = (aMovement, aStanding, aSpell, aAttack, aMineral, aDefault, aDeath);

type
  TAnimation = record
    state: eAnim;
    origin: Tpoint;
    frames, current, time: integer;
    increment, running, runonce: boolean;
  end;

type
  TSprite = class
  private
    active, frametime: integer;
    animset: array of TAnimation;
  public
    size: Tpoint;
    sheet: string;
    function position(): TPoint4;
    procedure update();
    procedure animate(state: eAnim; runonce: boolean = false);
    procedure animation(frames: integer; anitype: eAnim; x, y: integer);
    constructor create(sheet: string; width, heigth: integer);
  end;

implementation

uses System_Log;

// will play the first animation added by default
constructor TSprite.create(sheet: string; width, heigth: integer);
begin
  setLength(animset, 0);
  self.sheet := sheet;
  size.x := width;
  size.y := heigth;
  frametime := 10;
  active := 0;
end;

function TSprite.position(): TPoint4;
begin // + origin on all
  with (animset[active]) do
  begin
    result := Point4(origin.x + (current * size.x), origin.y, origin.x + size.x * (current + 1), origin.y,
      origin.x + size.x * (current + 1), origin.y + size.y, origin.x + current * size.x, origin.y + size.y);
    { result := Point4(origin.x + (current * size.x), origin.y + (current * size.y), origin.x + size.x * (current + 1),
      origin.y + size.y * current, origin.x + size.x * (current + 1), origin.y + size.y * (current + 1),
      origin.x + current * size.x, origin.y + size.y * (current + 1)); }
  end;
end;

procedure TSprite.animation(frames: integer; anitype: eAnim; x, y: integer);
var
  pos: integer;
begin
  pos := length(animset);
  setLength(animset, pos + 1);

  animset[pos].running := true;
  animset[pos].origin.x := x;
  animset[pos].origin.y := y;
  animset[pos].state := anitype;
  animset[pos].frames := frames;
  animset[pos].time := frametime;
  animset[pos].current := 0;
  animset[pos].increment := true;
end;

procedure TSprite.animate(state: eAnim; runonce: boolean = false);
var
  i: integer;
begin
  for i := 0 to length(animset) - 1 do
    if (animset[i].state = state) then
    begin
      active := i;
      animset[i].running := true;
      animset[i].runonce := runonce;

      if (runonce) then
      begin
        animset[i].current := 0;
        animset[i].increment := true;
      end;
    end;
end;

// if you want to do custom frames don't call TSprite.update.
procedure TSprite.update;
begin
  dec(animset[active].time);

  if (animset[active].time = 0) then
  begin
    animset[active].time := frametime;

    if (animset[active].running) then
      if (animset[active].increment) then
        inc(animset[active].current)
      else
        dec(animset[active].current);

    if (animset[active].current = 0) or (animset[active].current = animset[active].frames - 1) then
    begin
      animset[active].increment := not animset[active].increment;

      if (animset[active].runonce) then
        animset[active].running := false;
    end;
  end;
end;

end.