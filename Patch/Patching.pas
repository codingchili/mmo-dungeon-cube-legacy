unit Patching;

interface

uses System_Vars, SysUtils, Classes, Utils, Main, ShellApi, Windows, Dialogs;

var
  filecount, index: integer;
  filesize: int64;
  key, filename: string;

procedure patch;
procedure initialize;

implementation

uses Networking;

procedure initialize;
begin
  key := 'patch';
end;

procedure SaveVersion();
var
  versionfile: TextFile;
begin
  if not(DirectoryExists('conf')) then
    mkdir('conf');
  AssignFile(versionfile, 'conf/Ver.dat');
  rewrite(versionfile);
  writeln(versionfile, FloatToStr(update));
  closefile(versionfile);
end;

procedure GetMeta();
var
  i: integer;
begin
  for i := filecount - 1 downto 0 do
  begin
    dl.Socket.WriteLn(key + '&' + IntToStr(ord(ptpatch)) + '&' +
      IntToStr(ord(System_Vars.filesize)) + '&' + FloatToStr(current) + '&' +
      IntToStr(i), Packet.Encoding);

    setstatustext('Fetching Meta.. ' + IntToStr(i) + '/' + IntToStr(filecount));
    Main.ProcessMessages;

    Networking.filesizes[i] := dl.Socket.ReadInt64();
    Patching.filesize := Networking.filesizes[i] + Patching.filesize;
  end;
end;

procedure GetVersion();
begin
  dl.Socket.WriteLn(key + '&' + IntToStr(ord(ptpatch)) + '&' +
    IntToStr(ord(System_Vars.Version)), Packet.Encoding);

  update := SafeFloat(dl.Socket.ReadLn('Z', Packet.Encoding));
end;

procedure GetFileCount();
begin
  // get patch data, file count.
  dl.Socket.WriteLn(key + '&' + IntToStr(ord(ptpatch)) + '&' +
    IntToStr(ord(Count)) + '&' + FloatToStr(current), Packet.Encoding);

  filecount := StrToInt(dl.Socket.ReadLn('Z', Packet.Encoding));
end;

// holding shift while starting the patcher will reset patch version.
procedure Version();
var
  versionfile: TextFile;
  versiontext: string;
begin
  if not(fileexists('conf/Ver.dat')) or (GetASyncKeyState(VK_SHIFT) <> 0) then
    current := 0.0
  else
  begin
    AssignFile(versionfile, 'conf/Ver.dat');
    Reset(versionfile);
    readln(versionfile, versiontext);
    closefile(versionfile);
    current := SafeFloat(versiontext);
  end;
end;

procedure patch();
begin
  SetLength(filesizes, 0);
  Version();
  Networking.Connect();
  GetVersion();

  // if (dl.Connected) then
  // Authenticate();

  if (update > current) then
  begin
    GetFileCount();
    setstatustext('Updating ' + IntToStr(filecount) + ' Files..');
    SetLength(filesizes, filecount);

    GetMeta();
    index := filecount;

    PForm.loaded_all.Width := 0;
    starttick := getTickCount;
    lastwork := 0;

    // download outdated files.
    while (index > 0) do
    begin
      dec(index);
      Download(index);
    end;

    SaveVersion();
    setstatustext('        Updated to v' + FloatToStr(update));
  end
  else
    setstatustext('   Latest Version Installed.');

  setprogress(1000);
  Main.ProcessMessages;
  Sleep(200);
  ShellExecute(0, 'open', 'Lobby.exe', nil, nil, SW_SHOWNORMAL);
  Main.Terminate;

end;

end.
