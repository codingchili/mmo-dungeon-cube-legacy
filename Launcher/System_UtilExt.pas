unit System_UtilExt;

interface

uses SysUtils, Classes, JPeg, AsphyreJPG;

type
  TFileInfo = Record
    filename: string;
    filepath: string;
    filesize: integer;
    loaded: boolean;
  end;

type
  TFileControl = class
  public
    gamedata: boolean;
    Files: array of TFileInfo;
    datasize: integer;
    procedure Add(path, filename: string; size: int64);
    procedure Load(start: integer = 0);
    procedure FileSearch(const PathName, filename: string; const InDir: boolean);
    function id(filename: string): integer;
    function Unloaded(): integer;
    constructor create();
  end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings; preservespace: boolean = false);
function SafeFloat(pText: string): single;
procedure Initialize;

var
  FileMan: TFileControl;
  root: string;

implementation

uses Main, System_Log, Conf_Protocol;

constructor TFileControl.create;
begin
  gamedata := false;
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
begin
  SetLength(Files, length(Files) + 1);
  Files[High(Files)].filepath := path;
  Files[High(Files)].filename := filename;
  Files[High(Files)].filesize := size;
  Files[High(Files)].loaded := false;
  datasize := datasize + size;
end;

procedure TFileControl.Load(start: integer = 0);
var
  i: integer;
  loadcount, loaded: integer;
begin
  loadcount := Unloaded();
  loaded := 0;

  for i := start to High(Files) do
  begin
    if (Files[i].loaded = false) then
    begin
      Files[i].loaded := true;

      //GameImages.AddFromFileEx(Files[i].filepath + Files[i].filename, Files[i].filename);


     // print('Loaded [' + Files[i].filename + '] in ' + sw.Elapsed + ' ..', System_Log.White);
      inc(loaded);
    end;
  end;
end;

procedure TFileControl.FileSearch(const PathName, filename: string; const InDir: boolean);
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

function TFileControl.id(filename: string): integer;
var
  i, pos: integer;
begin
  pos := -1;
  for i := 0 to high(Files) do
    if filename = Files[i].filename then
      pos := i;

  result := pos;
end;

procedure Initialize;
begin
  FileMan := TFileControl.create();
  SetLength(FileMan.Files, 0);
  FileMan.FileSearch(root + 'media/gui', '*.png', true);
  FileMan.FileSearch(root + 'media/gui', '*.jpg', true);
  FileMan.Load;
end;

end.
