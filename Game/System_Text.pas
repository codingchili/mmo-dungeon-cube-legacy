unit System_Text;

// this is the battle log.

interface

uses SysUtils, SyncObjs, {$IFDEF Win32}Windows,{$ENDIF} Classes;

type
  TText = record
    color, base: integer;
    dir, speed, x, y: single;
    alpha, ttl: integer;
    value: string[12];
  end;

type
  TTextEngine = class
  private
    count: integer;
    cs: TCriticalSection;
  public
    Text: array of TText;
    procedure add(x, y: single; value: string; color: cardinal);
    procedure process();
    procedure render();
  end;

const
  FADE_IN = 24; // amount to fade in from 0 when added.
  FADE_OUT = 16; // amount to fade out when ttl expired per frame
  speed = 3.4; // initial speed.
  DECLINE = 0.94; // reduction in speed over time
  ttl = 42; // time before text fades
  OFFSET = 12; // pixel offset from center

procedure initialize;

var
  TextEngine: TTextEngine;

implementation

uses Engine_Player, System_Log, Vectors2, AsphyreTypes, System_Initializer, System_Camera;

procedure initialize;
begin
  TextEngine := TTextEngine.create;
  setlength(TextEngine.Text, 0);
  TextEngine.cs := TCriticalSection.create;
end;

procedure TTextEngine.render();
var
  i: integer;
begin
  cs.Acquire;

  for i := 0 to length(Text) - 1 do
  begin
    GameCanvas.FillRect(trunc(Text[i].x + cam.x - 3), trunc(Text[i].y + cam.y - 1),
      trunc(GameFonts.Items[0].TextWidth(Text[i].value)) + 7, 16, (Text[i].color and $FF000000));
  end;

  for i := 0 to length(Text) - 1 do
    GameFonts.Items[0].TextOut(point2(trunc(Text[i].x + cam.x), trunc(Text[i].y + cam.y)), Text[i].value,
      cColor2(Text[i].color, Text[i].color));
  cs.Release;
end;

procedure TTextEngine.add(x, y: single; value: string; color: cardinal);
var
  len: integer;
begin
  cs.Acquire;
  len := length(Text);
  setlength(Text, len + 1);

  Text[len].dir := (6.14 / 360) * random(360);
  Text[len].x := (x - 16) + OFFSET * Cos(Text[len].dir);
  Text[len].y := y + OFFSET * Sin(Text[len].dir);
  Text[len].value := value;
  Text[len].color := color;
  Text[len].speed := speed;
  Text[len].ttl := ttl;
  Text[len].alpha := 0;
  Text[len].base := color - $FF000000;
  cs.Release;
end;

procedure TTextEngine.process;
var
  len, i: integer;
begin
  cs.Acquire;
  len := length(Text);
  i := 0;

  while (i < len) do
  begin
    dec(Text[i].ttl);
    Text[i].x := Text[i].x + Text[i].speed * Cos(Text[i].dir);
    Text[i].y := Text[i].y + Text[i].speed * Sin(Text[i].dir);
    Text[i].speed := Text[i].speed * DECLINE;
    if (Text[i].speed < 0) then
      Text[i].speed := 0;

    if (Text[i].alpha < 255) and (Text[i].ttl > 0) then
      Text[i].alpha := Text[i].alpha + FADE_IN;
    if Text[i].alpha > 255 then
      Text[i].alpha := 255;

    if (Text[i].ttl < 0) and (Text[i].alpha > 0) then
      Text[i].alpha := Text[i].alpha - FADE_OUT;
    if Text[i].alpha < 0 then
      Text[i].alpha := 0;

    Text[i].color := Text[i].base + (Text[i].alpha shl 24);

    if (Text[i].alpha = 0) and (Text[i].ttl < 0) then
    begin
      cs.Acquire;

      if (len > 1) then
      begin
        Move(Text[i + 1], Text[i], SizeOf(TText) * (len - i));
        setlength(Text, len - 1);
      end
      else
        setlength(Text, 0);

      cs.Release;

      dec(len);
      dec(i);
    end;
    inc(i);
  end;
  cs.Release;
end;

end.