unit Effects_Blending;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses AsphyreTypes, SysUtils;

const
SPEED1 = $01000000;
SPEED2 = $03000000;
SPEED3 = $05000000;
SPEED4 = $0A000000;
SPEED5 = $12000000;
SPEED6 = $33000000;
SPEED7 = $55000000;

type
  TFadeMode = (FadeIn, FadeOut);

type
  TFade = Record
  public
    value, speed: cardinal;
    color: TColor4;
    fading: boolean;
    fademode: TFadeMode;
    procedure fade;
    procedure ins;
    procedure outs;
    procedure full;
    procedure empty;
    procedure seed(colors: cardinal);
  End;

implementation


procedure TFade.seed(colors: cardinal);
var
  i: integer;
begin
  for i := 0 to 3 do
    color[i] := colors;
end;

procedure TFade.full;
var
  i: integer;
begin
  value := $FF000000;
  for i := 0 to 3 do
    color[i] := $FF000000;
end;

procedure TFade.empty;
var
  i: integer;
begin
  value := $00000000;
  for i := 0 to 3 do
    color[i] := $00000000;
end;

procedure TFade.ins;
begin
  empty;
  speed := SPEED4;
  fademode := TFadeMode.FadeIn;
  fading := true;
end;

procedure TFade.outs;
begin
  full;
  speed := SPEED4;
  fading := true;
  fademode := TFadeMode.FadeOut;
end;

procedure TFade.fade;
var
  i: integer;
begin
  if fading then
  begin

    if (fademode = TFadeMode.FadeOut) then
    begin
      value := value - $0F000000;

      if (value = $00000000) then
        fading := false;
    end;

    if (fademode = TFadeMode.FadeIn) then
    begin
      value := value + $0F000000;

      if (value = $FF000000) then begin
        fading := false;
        FadeMode := TFadeMode.FadeOut;
      end;
    end;

    for i := 0 to 3 do
    begin
      color[i] := value;
    end;
  end;
end;

end.
