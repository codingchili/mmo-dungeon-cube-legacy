unit System_UtilExt;

interface

uses
{$IFDEF Win32} JPeg, AsphyreJPG, {$ENDIF}
{$IFDEF Linux}AsphyreVampyre, {$ENDIF}
  SysUtils, Classes, Dialogs, SyncObjs;

type
  TFileInfo = Record
    filename: string;
    filepath: string;
    filesize: integer;
    loaded: boolean;
  end;

type
  TFileControl = class
  private
    cs: TCriticalSection;
    procedure Add(path, filename: string; size: int64 = -1);
    function Unloaded(): integer;
  public
    gamedata: boolean;
    datasize: integer;
    Files: array of TFileInfo;
    procedure Load(start: integer = 0);
    procedure FileSearch(const PathName, filename: string; const InDir: boolean = false);
    constructor create();
  end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings; preservespace: boolean = false);
function SafeFloat(pText: string): single;
procedure Initialize;

var
  FileMan: TFileControl;
  root: string;

implementation

uses System_Initializer, Main, System_Log, Debug_StopWatch, Conf_Protocol;

constructor TFileControl.create;
begin
  SetLength(Files, 0);
  gamedata := false;
  cs := TCriticalSection.create;
end;

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

function SafeFloat(pText: string): single;
var
  is_euro: boolean;
  i: integer;
  dummy: single;
  dummy_str: string;
begin
  dummy := 1 / 2;
  dummy_str := FloatToStr(dummy);
  is_euro := false;

  for i := 0 to length(dummy_str) - 1 do
  begin
    if dummy_str[i] = '.' then
      is_euro := true;
    if dummy_str[i] = ',' then
      is_euro := false;
  end;

  if is_euro = false then
    pText := StringReplace(pText, '.', ',', [rfIgnoreCase, rfReplaceAll]);

  if is_euro = true then
    pText := StringReplace(pText, ',', '.', [rfIgnoreCase, rfReplaceAll]);

  result := StrToFloat(pText);
End;

function TFileControl.Unloaded(): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to high(Files) do
    if Files[i].loaded = false then
      inc(result);
end;

procedure TFileControl.Add(path, filename: string; size: int64);
var
  loaded: boolean;
  i: integer;
begin
  cs.Acquire;
  loaded := false;
  for i := 0 to high(Files) do
    if (Files[i].filename = filename) and (Files[i].filesize = size) then
      loaded := true;

  if not(loaded) then
  begin
    SetLength(Files, length(Files) + 1);
    Files[High(Files)].filepath := path;
    Files[High(Files)].filename := filename;
    Files[High(Files)].filesize := size;
    Files[High(Files)].loaded := false;
    datasize := datasize + size;
  end;
  cs.Release;
end;

procedure TFileControl.Load(start: integer = 0);
var
  i: integer;
  sw: TStopWatch;
  loadcount, loaded: integer;
begin
  sw := TStopWatch.create;

  // Main.RenderForm.loadbar.MaxValue := 100; // datasize;
  // Main.RenderForm.loadbar.Progress := 0;
  loadcount := Unloaded();
  loaded := 0;

  cs.Acquire;
  for i := start to High(Files) do
  begin
    if (Files[i].loaded = false) then
    begin
      Files[i].loaded := true;
      sw.start;
      GameImages.AddFromFileEx(Files[i].filepath + Files[i].filename, Files[i].filename);
      sw.Stop;

      print('Loaded [' + Files[i].filepath + Files[i].filename + '] in ' + sw.Elapsed + ' ..', System_Log.White);
      inc(loaded);
    end;
  end;
  cs.Release;

  sw.Free;
end;

procedure TFileControl.FileSearch(const PathName, filename: string; const InDir: boolean = false);
var
  Rec: TSearchRec;
  path: string;
begin
  path := { IncludeTrailingBackslash } (PathName) + '/';

  if FindFirst(path + filename, faAnyFile - faDirectory, Rec) = 0 then
    try
      repeat
        Add(path, Rec.Name, Rec.size);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;

  If not InDir then
    Exit;

  if FindFirst(path + '*.*', faDirectory, Rec) = 0 then
    try
      repeat
        if ((Rec.Attr and faDirectory) <> 0) and (Rec.Name <> '.') and (Rec.Name <> '..') then
          FileSearch(path + Rec.Name, filename, true);
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
end;

procedure Initialize;
begin
  FileMan := TFileControl.create();
end;

end.
