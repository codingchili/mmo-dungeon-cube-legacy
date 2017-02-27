unit Engine_Particles;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses SysUtils, Controls, Math, Debug_Stopwatch, SyncObjs, AbstractCanvas;

type
  TParticleEffect = (peSmoke, peHotcold, peFade, peDarken, peLighten, peSpectrum, peNormal, peFumeIfy, peLightning,
    peSplat, peAdd, peWater);

type
  TParticle = Record
  private
    x, y: single;
    color: cardinal;
    ttl: integer;
    dir, speed, size { , range } : single;
    fade: boolean;
    effect: TParticleEffect;
    blending: TBlendingEffect;
  End;

type
  TParticleMan = class
  private
    prt, buf: array of TParticle;
    cs: TCriticalSection;
    bufcs: TCriticalSection;
    sprite: integer;
  public
    constructor create();
    procedure Orphan();
    procedure Flushbuffer();
    procedure Movement();
    procedure Draw();
    procedure Fire(x, y, speed, dir, range: single; effect: TParticleEffect; color: cardinal; size: integer = 3);
    function prtCount(): integer;
  end;

procedure initialize;

var
  ParticleMan: TParticleMan;

implementation

uses Main, System_Initializer, System_Camera, Engine_Projectiles, AsphyreTypes, Conf_Spritemap,
  System_Log;

constructor TParticleMan.create;
begin
  setlength(prt, 0);
  setlength(buf, 0);
  cs := TCriticalSection.create;
  bufcs := TCriticalSection.create;
  sprite := -1;
end;

procedure initialize();
begin
  ParticleMan := TParticleMan.create;
end;

function TParticleMan.prtCount(): integer;
begin
  // cs.Acquire;
  result := length(prt);
  // cs.Release;
end;

procedure TParticleMan.Flushbuffer();
var
  buflen, i, len: integer;
begin
  bufcs.Acquire;
  buflen := length(buf);

  if (buflen > 0) then
  begin
    // cs.Acquire;
    len := length(prt);
    setlength(prt, length(prt) + buflen);

    for i := len to (len + buflen - 1) do
    begin
      prt[i] := buf[i - len];
    end;

    setlength(buf, 0);
    // cs.Release;
  end;
  bufcs.Release;
end;

procedure TParticleMan.Fire(x, y, speed, dir, range: single; effect: TParticleEffect; color: cardinal;
  size: integer = 3);
var
  pos: integer;
begin
  bufcs.Acquire;

  pos := length(buf);
  setlength(buf, pos + 1);

  buf[pos].x := x;
  buf[pos].y := y;
  buf[pos].speed := speed;
  buf[pos].dir := dir;
  buf[pos].effect := effect;
  buf[pos].color := color;
  buf[pos].size := size;
  buf[pos].fade := true;
  buf[pos].ttl := trunc(abs(range / speed));
  // print('range = ' + floattostr(range) + ' speed = ' + floattostr(speed));

  buf[pos].blending := TBlendingEffect.beAdd;

  // beShadow good for shadow, beNOrmal is good for color, beAdd is bad for bg
  if (buf[pos].effect = TParticleEffect.peSmoke) then
    buf[pos].blending := TBlendingEffect.beNormal;

  if (buf[pos].effect = TParticleEffect.peNormal) or (buf[pos].effect = TParticleEffect.peWater) then
    buf[pos].blending := TBlendingEffect.beNormal;

  bufcs.Release;
end;

procedure TParticleMan.Movement();
var
  len, i: integer;
begin
  // cs.Acquire;
  // effect();
  len := length(prt) - 1;

  For i := 0 to len do
  begin
    prt[i].x := prt[i].x + prt[i].speed * Cos(prt[i].dir);
    prt[i].y := prt[i].y + prt[i].speed * Sin(prt[i].dir);

    case prt[i].effect of
      TParticleEffect.peWater:
        begin
          prt[i].speed := prt[i].speed * 0.99835;
          prt[i].size := prt[i].size * 1.001;
          { if prt[i].fade then
            prt[i].color := prt[i].color - $00010100
            else
            prt[i].color := prt[i].color + $00010100;

            if (prt[i].color < $FF0101FF) then
            prt[i].fade := false;
            if (prt[i].color > $FF2828FF) then
            prt[i].fade := true; }
        end;
      TParticleEffect.peHotcold:
        begin
          prt[i].color := prt[i].color - $02010101;
          prt[i].speed := prt[i].speed * 1.0125;
          prt[i].ttl := trunc(prt[i].ttl * 0.9935);
        end;
      TParticleEffect.peFumeIfy:
        begin
          prt[i].color := prt[i].color - $02020000;
          prt[i].speed := prt[i].speed * 1.0125;
          prt[i].ttl := trunc(prt[i].ttl * 0.9935);
        end;
      TParticleEffect.peFade:
        begin
          if (prt[i].color > $01010000) then
            if (random(6) = 0) then
              prt[i].color := prt[i].color - $01010000;
          if (random(16) = 0) then
            prt[i].size := prt[i].size - 1;
          prt[i].speed := prt[i].speed * 1.0056;
          prt[i].dir := prt[i].dir + 6.14 / 1080;
        end;
      TParticleEffect.peSmoke:
        begin
          // if (random(2) = 0) then
          if (prt[i].size > 1) then
          begin
            prt[i].size := prt[i].size - 1;
            prt[i].x := prt[i].x + 0.5;
            prt[i].y := prt[i].y + 0.5;
          end;
          // if (random(4) = 0) then
          prt[i].color := prt[i].color - $01000000;
          prt[i].speed := prt[i].speed * 0.988;
          prt[i].dir := prt[i].dir + 6.14 / 1080;
        end;
    end;

    dec(prt[i].ttl);
  end;
  // cs.Release;
End;

procedure TParticleMan.Orphan();
var
  i, len: integer;
  trail: cardinal;
  df: boolean;
begin
  // cs.Acquire;
  len := length(prt);
  df := false;

  for i := 0 to len - 1 do
  begin
    if prt[i].ttl < 1 then
    begin
      df := true;
      trail := len - i;
      if trail > 0 then
        prt[i] := prt[len - 1];

      dec(len);
    end;
  end;

  if (df = true) then
    setlength(prt, len);
  // cs.Release;
end;

procedure TParticleMan.Draw;
var
  i: integer;
begin
  if sprite = -1 then
    sprite := GameImages.Resolve('controls.png');

  // cs.Acquire;
  for i := 0 to length(prt) - 1 do
  begin
    GameCanvas.UseImagePx(GameImages.Items[sprite], Point4(SPRITE_PARTICLE.x, SPRITE_PARTICLE.y, SPRITE_PARTICLE.x + 6,
      SPRITE_PARTICLE.y, SPRITE_PARTICLE.x + 6, SPRITE_PARTICLE.y + 6, SPRITE_PARTICLE.x, SPRITE_PARTICLE.y + 6));

    GameCanvas.TexMap(pBounds4(round(prt[i].x + cam.x), round(prt[i].y + cam.y), prt[i].size, prt[i].size),
      cColor4(prt[i].color), prt[i].blending);
  end;
  // cs.Release;
end;

end.
