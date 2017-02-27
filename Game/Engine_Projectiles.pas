unit Engine_Projectiles;

// fire projectiles.
{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses Conf_Protocol, SyncObjs, SysUtils, Math, DEBUG_STOPWATCH, Vectors2,
  AsphyreTypes, AbstractCanvas, {$IFDEF Win32}Windows, {$ENDIF} System_Spells, Types, Effects_Smoke, System_Keyboard;

const
  SPELL_SPRITESHEET: string = 'spellset.png';
  SPRITE_SIZE: integer = 32;

type
  TProjectile = Record
  public
    x, y, speed, dir, rot { , range } : single;
    mode: TSpellType;
    { id, } color, ttl, Id: integer; // the server may still reference projectiles.
    sprite: TPoint; // resource name. //replace with sprite
  End;

type
  TProjectileMan = class
  private
    prj, buf: array of TProjectile;
    cs, bufcs: TCriticalSection; // required - lock while drawing!
    sw: TStopWatch;
    spritesheet: integer;
  public
    constructor create();
    procedure fire(x, y, speed, dir: single; ttl: integer; sprite: TPoint; color: cardinal = $FFFF0000;
      prjId: integer = -1);
    procedure flushbuffer();
    procedure movement();
    procedure orphan();
    procedure draw();
    procedure sort(var arr: array of TProjectile; iLo, iHi: integer);
    function prjCount(): integer;
    procedure RemoveId(Id: integer);
  end;

procedure Initialize();

var
  ProjectileMan: TProjectileMan;

implementation

uses System_Initializer, System_camera, Engine_Particles, {$IFDEF Win32}System_Audio, {$ENDIF} System_Log;

procedure Initialize();
begin
  ProjectileMan := TProjectileMan.create;
end;

procedure TProjectileMan.RemoveId(Id: integer);
var
  i, len: integer;
begin
  cs.Acquire;
  len := length(prj) - 1;
  for i := 0 to len do
    if (prj[i].Id = Id) then
    begin
      prj[i].ttl := 0;
    end;
  cs.Release;
end;

function TProjectileMan.prjCount(): integer;
begin
  cs.Acquire;
  result := length(prj);
  cs.Release;
end;

constructor TProjectileMan.create;
begin
  sw := TStopWatch.create(false);
  setlength(prj, 0);
  setlength(buf, 0);
  cs := TCriticalSection.create;
  bufcs := TCriticalSection.create;
  spritesheet := -1;
end;

procedure TProjectileMan.sort(var arr: array of TProjectile; iLo, iHi: integer);
var
  Lo, Hi, Pivot: integer;
  T: TProjectile;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := ord(arr[(Lo + Hi) div 2].mode);
  repeat
    while ord(arr[Lo].mode) < Pivot do
      Inc(Lo);
    while ord(arr[Hi].mode) > Pivot do
      Dec(Hi);
    if Lo <= Hi then
    begin
      T := arr[Lo];
      arr[Lo] := arr[Hi];
      arr[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then
    sort(arr, iLo, Hi);
  if Lo < iHi then
    sort(arr, Lo, iHi);
end;

procedure TProjectileMan.flushbuffer();
var
  buflen, i, len: integer;
begin
  bufcs.Acquire;
  // sw.Start;
  buflen := length(buf);

  if buflen > 0 then
  begin
    cs.Acquire;
    len := length(prj);
    setlength(prj, length(prj) + buflen);

    for i := len to len + buflen - 1 do
      prj[i] := buf[i - len];

    sort(prj, low(prj), high(prj)); // change to INSERT method?
    setlength(buf, 0);
    cs.Release;
  end;
  // sw.Stop;
  bufcs.Release;
end;

procedure TProjectileMan.fire(x, y, speed, dir: single; ttl: integer; sprite: TPoint; color: cardinal = $FFFF0000;
  prjId: integer = -1);
var
  pos: integer;
begin
  bufcs.Acquire;

  pos := length(buf);
  setlength(buf, pos + 1);

  buf[pos].Id := prjId;
  buf[pos].x := x;
  buf[pos].y := y;
  buf[pos].speed := speed;
  buf[pos].dir := dir;
  buf[pos].sprite := sprite;
  buf[pos].color := color;
  buf[pos].ttl := ttl;

  bufcs.Release;
end;

procedure TProjectileMan.draw();
var
  theta, angle: single;
  origin, size, middle: TPoint2;
  len, i: integer;
begin
  if (spritesheet = -1) then
    spritesheet := GameImages.Resolve(SPELL_SPRITESHEET);

  len := length(prj) - 1;

  for i := 0 to len do
  begin
    GameCanvas.UseImagePx(GameImages.Items[spritesheet], Point4(prj[i].sprite.x, prj[i].sprite.y,
      prj[i].sprite.x + SPRITE_SIZE, prj[i].sprite.y, prj[i].sprite.x + SPRITE_SIZE, prj[i].sprite.y + SPRITE_SIZE,
      prj[i].sprite.x, prj[i].sprite.y + SPRITE_SIZE));

    origin := point2(trunc(prj[i].x + cam.x), trunc(prj[i].y + cam.y));
    size := point2(24, 24);
    middle := point2(12, 12);
    theta := 1;
    angle := DegToRad(RadToDeg(prj[i].dir) + 90 + prj[i].rot);

    GameCanvas.TexMap(pRotate4(origin, size, middle, angle, theta), cColor4($FFFFFFFF), TBlendingEffect.beNormal);

    GameCanvas.UseImagePx(GameImages.Items[spritesheet], Point4(prj[i].sprite.x, prj[i].sprite.y,
      prj[i].sprite.x + SPRITE_SIZE, prj[i].sprite.y, prj[i].sprite.x + SPRITE_SIZE, prj[i].sprite.y + SPRITE_SIZE,
      prj[i].sprite.x, prj[i].sprite.y + SPRITE_SIZE));

    GameCanvas.TexMap(pRotate4(point2(prj[i].x + cam.x + 14, prj[i].y + cam.y + 14), point2(16, 16), point2(8, 8),
      angle, theta), cColor4($66000000), TBlendingEffect.beNormal);
  end;
end;

procedure TProjectileMan.movement();
var
  len, i, k, probability: integer;
begin
  len := length(prj) - 1;
  probability := random(30); // twice per frame

  For i := 0 to len do
  begin
    prj[i].x := prj[i].x + prj[i].speed * Cos(prj[i].dir);
    prj[i].y := prj[i].y + prj[i].speed * Sin(prj[i].dir);
    Dec(prj[i].ttl);

    // onstealth/leavestealth
    // SmokeMan.Add(trunc(prj[i].x), trunc(prj[i].y), 10, $AA000000);

    if Keyboard.chars[48] then
     if probability > 0 then // smoke
      ParticleMan.fire(prj[i].x - 30, prj[i].y - 30, prj[i].speed / 10 +  1.08,
      DegToRad(RadToDeg(prj[i].dir) + random(360)), 70, TParticleEffect.peSmoke, $AA000000, random(40) + 50);

      if Keyboard.chars[49] then
     if probability > 26 then // loot glow
      ParticleMan.fire(prj[i].x, prj[i].y, prj[i].speed / 10 + 0.22, DegToRad(RadToDeg(prj[i].dir) + random(90) + 225 + 90), 30,
      TParticleEffect.peHotCold, $CCFFDF00, random(2) + 1);

      if Keyboard.chars[50] then
   if probability > 15 then // physical trail
      ParticleMan.fire(prj[i].x, prj[i].y, 0.32, DegToRad(RadToDeg(prj[i].dir) + random(360)), 40,
      TParticleEffect.peFumeIfy, $CCFFFFFF, 3);

      if Keyboard.chars[51] then
     if probability > 0 then // color ray? idk
      ParticleMan.fire(prj[i].x, prj[i].y, 0.86, DegToRad(RadToDeg(prj[i].dir) + random(90) - 45), 240,
      TParticleEffect.peAdd, $66000000 + random($FFFFFF), random(26) + 16);

      if Keyboard.chars[52] then
   if probability > 16 then
      begin // debuff cloud poison/bleed, drawn relative to player x for stick?
      ParticleMan.fire(prj[i].x, prj[i].y, 0.096, DegToRad(RadToDeg(prj[i].dir) + random(360)), 30,
      TParticleEffect.peFumeIfy, $18FF0000, random(6) + 6);
      end;

      if Keyboard.chars[53] then
     if probability > 26 then  //weapon glow?
      begin // debuff cloud poison/bleed, drawn relative to player x for stick?
      ParticleMan.fire(prj[i].x, prj[i].y, 0.096, DegToRad(RadToDeg(prj[i].dir) + random(360)), 30,
      TParticleEffect.peFumeIfy, $18FF0000, random(4) + 1);
      end;

      if Keyboard.chars[54] then
     if probability > 26 then // weapon glow?
      begin // debuff cloud poison/bleed, drawn relative to player x for stick?
      ParticleMan.fire(prj[i].x, prj[i].y, 0.096, DegToRad(RadToDeg(prj[i].dir) + random(360)), 30,
      TParticleEffect.peHotcold, $18FF000000, random(4) + 1);
      end;

      if Keyboard.chars[55] then
    if probability > 26 then // weapon glow?
    begin // debuff cloud poison/bleed, drawn relative to player x for stick?
      ParticleMan.fire(prj[i].x, prj[i].y, 0.096, DegToRad(RadToDeg(prj[i].dir) + random(360)), 30,
        TParticleEffect.peFumeIfy, $180000FF, random(4) + 1);
    end;


    if Keyboard.chars[56] then
     if probability > 0 then // portal?
     ParticleMan.fire(prj[i].x, prj[i].y, 0.0965, DegToRad(RadToDeg(prj[i].dir) + random(360)), 120,
     TParticleEffect.peFade, $44FF0000, random(18) + 18);

    // buffs are glittery/smh

  end;
End;

// explode particle
procedure TProjectileMan.orphan();
var
  i, j, len, trail: integer;
begin
  len := length(prj);

  for i := 0 to len - 1 do
  begin
    if prj[i].ttl < 1 then
    begin
      for j := 0 to 24 do
        ParticleMan.fire(prj[i].x, prj[i].y, prj[i].speed * 0.75,
          (6.28 / 360) * ( { random(60) } (j * 2 + 6) + RadToDeg(prj[i].dir) - 30), 60, TParticleEffect.peNormal,
          prj[i].color);

      trail := len - i - 1;
      if trail > 0 then
        Move(prj[i + 1], prj[i], SizeOf(TProjectile) * (trail));
      Dec(len);
    end;
  end;

  setlength(prj, len);
end;

end.
