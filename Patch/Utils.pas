unit Utils;

interface

Uses SysUtils, Classes;

function SafeFloat(pText: string): double;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings; preservespace: boolean = false);

implementation

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings; preservespace: boolean = false);
var
  i: integer;
begin
  if preservespace then
    Str := StringReplace(Str, ' ', '¤', [rfReplaceAll, rfIgnoreCase]);

  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.DelimitedText := Str;

  if preservespace then
    for i := 0 to ListOfStrings.Count - 1 do
      ListOfStrings[i] := StringReplace(ListOfStrings[i], '¤', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

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
