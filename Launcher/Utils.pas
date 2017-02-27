unit Utils;

interface

Uses SysUtils;

function SafeFloat(pText: string): double;

implementation

function SafeFloat(pText: string): double;
var
  is_euro: boolean;
  i: Integer;
  dummy: double;
  dummy_str: string;
begin
  dummy := 1 / 2;
  dummy_str := FloatToStr(dummy); // swap depending on host machine

  for i := 0 to length(dummy_str) - 1 do
  begin
    if dummy_str[i] = '.' then
      is_euro := True;
    if dummy_str[i] = ',' then
      is_euro := false;
  end;

  if is_euro = false then
    pText := StringReplace(pText, '.', ',', [rfIgnoreCase, rfReplaceAll]);

  if is_euro = True then
    pText := StringReplace(pText, ',', '.', [rfIgnoreCase, rfReplaceAll]);

  result := StrToFloat(pText);
End;

end.
