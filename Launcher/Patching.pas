unit Patching;

interface

uses System_Vars, SysUtils, Classes, Utils, Main, ShellApi, Windows, Dialogs,
  IdTCPClient, IdSocketHandle, IdGlobal, SyncObjs, Conf_protocol;

type
  TStatus = record
  public
    filecount, index, percent, lastwork, filework, completed: integer;
    filesizes, filesize, rate: int64;
    filename, bps, text, downloaded: string;
    done, finished: boolean;
  end;

type
  TPatcher = class(TThread)
  protected
    procedure Execute; Override;
  private
    dl: TIdTCPClient;
    current: double;
    tick: int64;
    cs: TCriticalSection;
  public
    update: double;
    newlicense, newpatch: boolean;
    status: TStatus;
    filesizes: array of integer;
    procedure unlock();
    procedure lock();
    constructor create;
    procedure DownloadFileCount();
    procedure version();
    procedure getversion();
    procedure Connect(reconnect: integer = 10);
    procedure getmeta();
    procedure download(index: integer);
    procedure saveversion();
    procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
  end;

type
  TPacket = class
  public
    param: TStringList;
    data: string;
    id: string[25];
    action: TAction;
    types: TPackType;
    ip: string[18];
    port: integer;
       class function Encoding(): {$IFDEF Win32}TEncoding{$ELSE}TIdEncoding{$ENDIF};
  end;

procedure Initialize;

var
  Patcher: TPatcher;

implementation

class function TPacket.Encoding(): {$IFDEF Win32}TEncoding{$ELSE}TIdEncoding{$ENDIF};
begin
   {$IFDEF Win32}
   result := TEncoding.UTF8;
   {$ELSE}
   result := TIdEncoding.enUTF8;{$ENDIF};
end;

procedure TPatcher.lock();
begin
  cs.acquire;
end;

procedure TPatcher.unlock;
begin
  cs.Release;
end;

procedure TPatcher.Execute;
begin
  SetLength(filesizes, 0);
  version();
  Connect();
  getversion();

  if (update > current) then
  begin
    Main.pForm.bar1unl.visible := true;
    Main.pForm.bar2unl.visible := true;
    Main.pForm.bar1ld.visible := true;
    Main.pForm.bar2ld.visible := true;
    pForm.transferspeed.visible := true;
    pForm.transferred.visible := true;

    DownloadFileCount();
    status.text := ('Updating ' + inttostr(status.filecount) + ' Files..');
    SetLength(filesizes, status.filecount);

    getmeta();
    status.index := status.filecount;

    status.completed := 0;
    starttick := getTickCount;
    sw.Start;
    sleep(1);
    // download outdated files.
    while (status.index > 0) do
    begin
      lock();
      dec(status.index);
      status.filework := 0;
      Patcher.status.filesize := filesizes[status.index];
      unlock();
      download(status.index);
    end;

    saveversion();
    status.text := ('        Updated to v' + FloatToStr(update));
  end
  else
    status.text := ('   Latest Version Installed.');

  pForm.transferred.visible := false;
  status.finished := true;

  while (true) do
    sleep(5000);
end;

procedure TPatcher.Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.DelimitedText := Str;
end;

constructor TPatcher.create();
begin
  inherited create;
  cs := TCriticalSection.create();
  dl := TIdTCPClient.create();
  dl.HOST := HOST;
  dl.port := port;
  dl.UseNagle := false;
  dl.OnWorkBegin := Main.netGUI.IdTCPClient1WorkBegin;
  dl.OnWork := Main.netGUI.IdTCPClient1Work;
  dl.OnWorkEnd := Main.netGUI.IdTCPClient1WorkEnd;
  dl.ConnectTimeout := 2250;
  dl.ReadTimeout := 2500;
  status.index := 0;
  current := 0.0;
  newlicense := false;
  newpatch := false;
end;

procedure TPatcher.Connect(reconnect: integer = 10);
var
  waiting: integer;
begin
  waiting := 0;

  lock();
  status.text := ('Connecting ..');
  unlock();
  try
    dl.Disconnect;
  except
  end;

  try
    dl.Connect;
    dl.Socket.RecvBufferSize := 8192; // default is 32.768
  except
  end;

  while (dl.Connected = false) do
  begin
    lock();
    status.text := ('Reconnecting in ' + inttostr(reconnect - waiting) + ' ..');
    unlock();
    sleep(1000);

    if (waiting = reconnect - 1) then
    begin
      waiting := -1;
      lock();
      status.text := ('Connecting ..');
      unlock();
      try
        dl.Connect;
        dl.Socket.RecvBufferSize := 50;
      except
        // On E: Exception do
        // showmessage(E.Message);
      end;
    end;

    inc(waiting);
  end;
end;

procedure TPatcher.download(index: integer);
var
  afile: TFileStream;
  Packet: TPacket;
begin
  status.done := false;
  status.filework := 0;
  repeat
    try
      dl.Socket.WriteLn('' + '&' + inttostr(ord(TPackType.Patch)) + '&' + inttostr(ord(TActionFileData)) + '&' +
        FloatToStr(current) + '&' + inttostr(index), TPacket.Encoding);


      lock();
      status.filename := dl.Socket.ReadLn('Z', TPacket.Encoding);
      status.filename := StringReplace(status.filename, '/', '\', [rfReplaceAll, rfIgnoreCase]);
      ForceDirectories(GetCurrentDir + '\' + ExtractFilePath(status.filename));

      if (status.filename = 'conf\license.txt') then
        newlicense := true;
      if (status.filename = 'conf\patch.txt') then
        newpatch := true;

      status.text := status.filename + '.. [' + inttostr(index) + '/' + inttostr(status.filecount) + ']';
      unlock();

      if fileexists(status.filename) then
        DeleteFile(PChar(status.filename));

      afile := TFileStream.create(status.filename, fmCreate);
      dl.Socket.ReadStream(afile, filesizes[index]);
      lock();
      status.done := true;
      unlock();
    except
      lock();
      Patcher.status.completed := Patcher.status.completed - status.filework;
      // reset progress
      status.filework := 0;
      status.text := ('Disconnected during transfer.');
      unlock();
      sleep(1500);
      Connect(3);
      // download(index); // restart, index hasn't changed yet.
    end;
    FreeAndNil(afile);
  until status.done;
end;

procedure Initialize;
begin
  Patcher := TPatcher.create;
end;

procedure TPatcher.saveversion();
var
  versionfile: textFile;
begin
  if not(directoryexists('conf')) then
    mkdir('conf');
  AssignFile(versionfile, 'conf/version.dat');
  rewrite(versionfile);
  writeln(versionfile, FloatToStr(update));
  closefile(versionfile);
end;

procedure TPatcher.getmeta();
var
  i: integer;
begin
  for i := status.filecount - 1 downto 0 do
  begin
    dl.Socket.WriteLn('&' + inttostr(ord(TPackType.Patch)) + '&' + inttostr(ord(System_Vars.filesize)) + '&' +
      FloatToStr(current) + '&' + inttostr(i), TPacket.Encoding);

    lock();
    status.text := ('Fetching Meta.. ' + inttostr(status.filecount - i) + '/' + inttostr(status.filecount));
    // Main.ProcessMessages;

    filesizes[i] := dl.Socket.ReadInt64();
    status.filesizes := filesizes[i] + status.filesizes;
    unlock();
  end;
end;

procedure TPatcher.getversion();
begin
  dl.Socket.WriteLn('&' + inttostr(ord(TPackType.Patch)) + '&' + inttostr(ord(System_Vars.version)),TPacket.Encoding);

  update := SafeFloat(dl.Socket.ReadLn('Z', TPacket.Encoding));
end;

procedure TPatcher.DownloadFileCount();
begin
  // get patch data, file count.
  dl.Socket.WriteLn('&' + inttostr(ord(TPackType.Patch)) + '&' + inttostr(ord(count)) + '&' + FloatToStr(current),
   TPacket.Encoding);

  lock();
  status.filecount := StrToInt(dl.Socket.ReadLn('Z', TPacket.Encoding));
  unlock();
end;

// holding shift while starting the patcher will reset patch version.
procedure TPatcher.version();
var
  versionfile: textFile;
  versiontext: string;
begin
  if not(fileexists('conf/version.dat')) or (getAsynckeystate(VK_SHIFT) <> 0) then
    current := 0.0
  else
  begin
    AssignFile(versionfile, 'conf/version.dat');
    Reset(versionfile);
    readln(versionfile, versiontext);
    closefile(versionfile);
    current := SafeFloat(versiontext);
  end;
end;

end.
