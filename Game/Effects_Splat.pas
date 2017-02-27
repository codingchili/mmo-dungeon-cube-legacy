unit Effects_Splat;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

uses {$IFDEF Win32}Windows,{$ENDIF} SysUtils, Math, System_Keyboard;

// use splats to manage day/night, draw torches/light-sources before/after

type
  TSplatEffect = (BloodSplat, PoisonSplat, StunSplat, LightningSplat, SlowSplat, BlindSplat);

type
  TSplat = record
    color: cardinal;
    ttl: integer;
    effect: TSplatEffect;
  end;

type
  TSplatMan = class
  public
    tick: integer;
    splats: array [0 .. 5] of TSplat;
    procedure Splat(effect: TSplatEffect);
    procedure Process();
    procedure Draw();
    constructor create();
  end;

procedure Initialize;

var
  SplatMan: TSplatMan;
  lock: integer; // remove

implementation

uses Engine_Particles, Engine_Player, System_Camera, System_Initializer, AbstractCanvas, System_Log, GUI_Chat;

procedure Initialize;
begin
  SplatMan := TSplatMan.create;
end;

constructor TSplatMan.create;
var
  Splat: TSplatEffect;
begin
  for Splat := low(TSplatEffect) to high(TSplatEffect) do
  begin
    splats[ord(Splat)].ttl := 0;
    splats[ord(Splat)].effect := Splat;
  end;
end;

procedure TSplatMan.Process;
var
  i: integer;
begin
  dec(lock);

  if lock < 0 then
  begin
    if (Keyboard.GetAsyncKeyState(ord('5')) <> 0) then
      Splat(BlindSplat);

    if (Keyboard.GetAsyncKeyState(ord('6')) <> 0) then
      Splat(BloodSplat);

    if (Keyboard.GetAsyncKeyState(ord('7')) <> 0) then
      Splat(PoisonSplat);

    if (Keyboard.GetAsyncKeyState(ord('8')) <> 0) then
      Splat(SlowSplat);

    if (Keyboard.GetAsyncKeyState(ord('9')) <> 0) then
      Splat(StunSplat);
  end;

  for i := low(splats) to high(splats) do
    if (splats[i].ttl > 0) then
    begin
      case (splats[i].effect) of
        BloodSplat:
          splats[i].color := splats[i].color - $02020000;
        PoisonSplat:
          splats[i].color := splats[i].color - $02000200;
        SlowSplat:
          splats[i].color := splats[i].color - $02000202;
        BlindSplat:
          splats[i].color := splats[i].color - $03000000;
      end;

      splats[i].ttl := splats[i].ttl - 1;
    end;

end;

procedure TSplatMan.Splat(effect: TSplatEffect);
var
  Splat: TSplat;
  i: integer;
begin
  lock := 60;
  Splat.effect := effect;
  Splat.ttl := 66;

  case (effect) of
    BloodSplat:
      Splat.color := $85850000;
    PoisonSplat:
      Splat.color := $85008500;
    SlowSplat:
      Splat.color := $85008585;
    BlindSplat:
      begin
        Splat.color := $FF000000;
        Splat.ttl := 55;
      end;
  end;

  for i := low(splats) to high(splats) do
    if (splats[i].effect = Splat.effect) then
    begin
      splats[i] := Splat;
      break;
    end;
end;


procedure TSplatMan.Draw(); // set draw to daytime sometimes
var
  i: integer;
begin
  // draw splats to screen
  for i := low(splats) to high(splats) do
    if (splats[i].ttl > 0) then
      GameCanvas.FillRect(0, 0, 3000, 3000, splats[i].color, TBlendingEffect.beNormal);

end;

end.