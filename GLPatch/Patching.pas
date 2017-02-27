unit Patching;

interface

uses SysUtils, Classes, Main, Dialogs, {$IFDEF Win32} Windows, ShellApi,
    TimeSpan, {$ENDIF}
    IdTCPClient, IdSocketHandle, IdGlobal, SyncObjs, Conf_Protocol, IdComponent,
    Debug_Stopwatch;

type
    TStatus = record
    public
        filecount, index, percent, lastwork, filework, completed: integer;
        filesizes, filesize, rate: int64;
        filename, bps, text, downloaded: string;
        done, finished, disconnected: boolean;
    end;

type
    TNetWorkerGUI = class
    public
        class procedure IdTCPClient1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
        procedure IdTCPClient1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
        procedure IdTCPClient1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    private
    end;

type
    FileMeta = record
        name: string;
        size: int64;
        version: double;
        index: integer;
    end;

type
    TPatcher = class(TThread)
    protected
        procedure Execute; Override;
    private
        dl: TIdTCPClient;
        tick, lastwork, starttick: int64;
        cs: TCriticalSection;
        filedone: boolean;
        mainpageloaded, errorlevel: boolean;
        netGUI: TNetWorkerGUI;
    public
        current: double;
        update: double;
        newlicense, newpatch: boolean;
        status: TStatus;
        files: array of FileMeta;
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

function RateFormat(rate: integer): string;
function SafeFloat(pText: string): single;
procedure Initialize;

var
    Patcher: TPatcher;
    sw: TStopWatch;
    lastwork: int64;
    params: string;

implementation

uses Settings_Patcher, System_Log;

function RateFormat(rate: integer): string;
// var
// rate: double;
begin
    // rate := ((AWorkCount - lastwork) * (sw.fFrequency / (sw.ElapsedTicks + 1)) / 1024);
    result := '';
    rate := round(rate / 1024);

    if rate < 1000 then
        result := FormatFloat('0', rate) + ' KB/s'
    else
        result := FormatFloat('0.00', rate / 1024) + ' MB/s';
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
    lock();
    status.done := false;
    status.finished := false;
    status.disconnected := true;
    unlock();

    params := '';
    SetLength(files, 0);
    version();
    Connect();
    lock();
    status.text := 'Checking for Updates..';
    unlock();
    getversion();

    if (update > current) then
    begin
        getmeta();
        lock();
        status.index := status.filecount;
        unlock();

        status.completed := 0;
        sw.Start;

        // download outdated files.
        while (status.index > 0) and not(terminated) do
        begin
            lock();
            dec(status.index);
            status.filework := 0;
            Patcher.status.filesize := files[status.index].size;
            unlock();
            download(files[status.index].index);
        end;

        if not(terminated) then
        begin
            saveversion();
            status.finished := true;
            status.text := ('Updated to v' + FormatFloat('0.0##', update));
            sleep(2500);
            status.done := true;
        end;
    end
    else
        status.text := ('Latest Version Installed.');

    if not(terminated) then
    begin

        status.completed := 100;
        status.filesizes := 100;
        status.done := true;

        if (newpatch) then
            params := params + #13 + #10 + '-patch';
        if (newlicense) then
            params := params + #13 + #10 + '-license';

        params := params + #13 + #10 + '-version:' + FormatFloat('0.###', self.update);

        if (Settings.option.autostart) then
        begin
            ShellExecute(0, 'open', 'Game.exe', PChar(params), NIL, SW_SHOWNORMAL);

{$IFDEF Win32}
            ExitProcess(0);
{$ELSE}
            Halt;
{$ENDIF}
        end;
    end;
    status.finished := true;
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
    netGUI := TNetWorkerGUI.create;
    dl.HOST := Settings.option.HOST;
    dl.PORT := Settings.option.PORT;
    dl.UseNagle := false;
    dl.OnWorkBegin := netGUI.IdTCPClient1WorkBegin;
    dl.OnWork := netGUI.IdTCPClient1Work;
    dl.OnWorkEnd := netGUI.IdTCPClient1WorkEnd;
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
    dl.HOST := Settings.option.HOST;
    dl.PORT := Settings.option.PORT;
    lock();
    status.disconnected := true;
    unlock();
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
        dl.Socket.RecvBufferSize := 2048; // 8192; // default is 32.768
    except
    end;

    while (dl.Connected = false) and not(terminated) do
    begin
        dl.HOST := Settings.option.HOST;
        dl.PORT := Settings.option.PORT;
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
                dl.Socket.RecvBufferSize := 2048;
            except
                // On E: Exception do
                // showmessage(E.Message);
            end;
        end;

        inc(waiting);
    end;

    lock();
    status.disconnected := false;
    unlock();
end;

procedure TPatcher.download(index: integer);
var
    afile: TFileStream;
    Packet: TPacket;
begin
    Packet := TPacket.create;
    filedone := false;
    status.filework := 0;
    repeat
        try
            Packet.types := TPackType.Patch;
            Packet.action := TAction.FileData;
            Packet.Add(inttostr(index));
            dl.Socket.Write(Packet.packetize);

            status.filename := files[status.index].name;
            lock();
            status.filename := StringReplace(status.filename, '/', '\', [rfReplaceAll, rfIgnoreCase]);
            unlock();

            ForceDirectories(GetCurrentDir + '\' + ExtractFilePath(status.filename));

            if (status.filename = 'conf\license.txt') then
                newlicense := true;
            if (status.filename = 'conf\patch.txt') then
                newpatch := true;

            lock();
            status.text := status.filename + '.. [' + inttostr(status.filecount - index) + '/' +
              inttostr(status.filecount) + ']';
            unlock();

            if fileexists(status.filename) then
                DeleteFile(PChar(status.filename));

            afile := TFileStream.create(files[index].name, fmCreate);
            dl.Socket.ReadStream(afile, files[index].size);
            lock();
            filedone := true;
            unlock();
        except
            lock();
            Patcher.status.completed := Patcher.status.completed - status.filework;
            // reset progress
            status.filework := 0;
            unlock();

            if not(terminated) then
            begin
                lock();
                status.text := ('Disconnected during transfer.');
                status.disconnected := true;
                unlock();
                sleep(1500);
                Connect(3);
            end
            else
                filedone := true; // pls no break must free
            ;
        end;

        FreeAndNil(afile);
    until filedone;
end;

procedure Initialize;
begin
    sw := TStopWatch.create();
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
    Packet: TPacket;
    Data: TIdBytes;
    len: word;
    i: integer;
begin
    lock();
    status.text := 'Fetching Meta..';
    unlock();

    Packet := TPacket.create;
    Packet.types := TPackType.Patch;
    Packet.action := TAction.filesize;
    Packet.Add(FloatToStr(current));
    try
        dl.Socket.Write(Packet.packetize);

        dl.Socket.ReadBytes(Data, 2);
        Move(Data[0], len, SizeOf(len));
        SetLength(Data, 0);

        dl.Socket.ReadBytes(Data, len);
        Packet.unpacketize(BytesToString(Data, IndyTextEncoding(Packet.Encoding)));

        status.filecount := StrToInt(Packet.param(0));
        SetLength(files, status.filecount);

        for i := 0 to status.filecount - 1 do
        begin
            files[i].name := Packet.param(1 + i * 4);
            files[i].version := SafeFloat(Packet.param(2 + i * 4));
            files[i].size := StrToInt(Packet.param(3 + i * 4));
            files[i].index := StrToInt(Packet.param(4 + i * 4));
            status.filesizes := status.filesizes + files[i].size;
        end;

    finally
        Packet.free;
    end;
end;

procedure TPatcher.getversion();
var
    Packet: TPacket;
begin
    Packet := TPacket.create;
    Packet.types := TPackType.Patch;
    Packet.action := TAction.version;
    try
        dl.Socket.Write(Packet.packetize);

        update := SafeFloat(dl.Socket.ReadLn('Z', IndyTextEncoding(Packet.Encoding)));
    finally
        Packet.free;
    end;
end;

procedure TPatcher.DownloadFileCount();
var
    filecount: integer;
    Packet: TPacket;
begin
    Packet := TPacket.create;
    Packet.types := TPackType.Patch;
    Packet.action := TAction.count;
    Packet.Add(FloatToStr(current));

    try
        dl.Socket.Write(Packet.packetize);
    finally
        Packet.free;
    end;

    filecount := StrToInt(dl.Socket.ReadLn('Z', IndyTextEncoding(Packet.Encoding)));

    lock();
    status.filecount := filecount;
    unlock();
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

class procedure TNetWorkerGUI.IdTCPClient1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
    delta: int64;
begin
    if (Patcher.terminated) then
    begin
        Patcher.dl.Disconnect;
        exit;
    end;

    try
        sw.Stop;
        delta := sw.ElapsedTicks;
        sw.Start;

        Patcher.lock;
        Patcher.status.completed := Patcher.status.completed + AWorkCount - lastwork;
        Patcher.status.filework := Patcher.status.filework + AWorkCount - lastwork;
        Patcher.unlock;

        if delta = 0 then
            delta := 1;

        Patcher.lock;
        Patcher.status.rate := round((AWorkCount - lastwork) / (delta / System.TimeSpan.TTimeSpan.TicksPerSecond));

        Patcher.status.downloaded := FormatFloat('0.00', Patcher.status.completed / 1024 / 1024) + ' of ' +
          FormatFloat('0.00', Patcher.status.filesizes / 1024 / 1024) + 'MB';
        Patcher.unlock;

        lastwork := AWorkCount;
        // tick := GetTickCount;
    except
        on E: Exception do
            showmessage('TNetWorker.OnWork:' + E.Message);
    end;
end;

procedure TNetWorkerGUI.IdTCPClient1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
begin
    lastwork := 0;
end;

procedure TNetWorkerGUI.IdTCPClient1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
    lastwork := 0;
end;

end.
