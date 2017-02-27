unit Network_WorldServer;

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses System_Log, Classes,
    SysUtils, System_UtilExt, IdTCPServer,
    IdSocketHandle, IdContext, IdTCPClient, Conf_Protocol, Engine_Characters,
    Network_World, Conf_WorldServer, SyncObjs, Engine_Accounts, IdGlobal, Engine_World, Debug_StopWatch, IdSchedulerOfThreadPool;

type
    TSyncListener = class(TThread)
    private
        client: TIdTCPClient;
        cs: TCriticalSection;
    protected
        procedure Execute; override;
    public
        constructor Create(var aclient: TIdTCPClient; var acs: TCriticalSection);
    end;

    // packet switch into the world threads.
type
    TWorldServer = class
    protected
        class procedure OnRead(AContext: TIdContext);
        class procedure OnDisconnect(Context: TIdContext);
        class procedure OnConnect(Context: TIdContext);
        class procedure LostMaster(Sender: TObject);
        class procedure pingpacket(var Context: TIdContext);
        class procedure SendCharacters(packet: TPacket; var Context: TIdContext);
        class procedure LoadWorld(var character: TCharacter; var Context: TIdContext; port: integer);
        class procedure LoginCharacter(packet: TPacket; var Context: TIdContext);
        class procedure NameCharacter(packet: TPacket; var Context: TIdContext);
        class procedure TokenExpired(var Context: TIdContext; const sessid: string);
    private
        cs: TCriticalSection;
        Server: TIdTCPServer;
        client: TIdTCPClient;
        worlds: array of TWorld;
        procedure WorldInit();
        constructor Create();
    public
        WorldConf: TWorldConf;
        procedure UpdateServerStatus(disconnection: boolean = False);
        function Online(): integer;
        function Registered(): integer;
    end;

procedure Initialize;

var
    WorldServer: TWorldServer;
    SyncListener: TSyncListener;

implementation

uses System_Token;

constructor TSyncListener.Create(var aclient: TIdTCPClient; var acs: TCriticalSection);
begin
    inherited Create(False);
    cs := acs;
    client := aclient;
    // FreeOnTerminate := True;
end;

procedure TSyncListener.Execute;
var
    packet: TPacket;
    data: TIdBytes;
    len: word;
begin
    // read sync indata
    while client.Connected do
    begin
        sleep(1);
        if client.Socket.InputBuffer.Size > 0 then
        begin
            packet := TPacket.Create;
            cs.Acquire;
            try
                // get message length
                client.Socket.ReadBytes(data, 2);
                Move(data[0], len, SizeOf(len));
                SetLength(data, 0);

                // verify packet size.
                if (len > Conf_Protocol.MAXBYTES) or (len < 0) then
                begin
                    // drop all bytes in buffer.
                    client.Socket.InputBuffer.Clear;
                    // client.Socket.DiscardAll;
                end
                else
                begin

                    // reset buffer
                    SetLength(data, 0);

                    // get message
                    client.Socket.ReadBytes(data, len);

                    // parse and unpack
                    packet.unpacketize(BytesToString(data, IndyTextEncoding(TPacket.Encoding)));

                    // reset the buffer again
                    SetLength(data, 0);

                    TokenDB.Read(packet);
                end;
            finally
                cs.Release;
                packet.Free;
            end;
        end;
    end;
end;

// used for character login-join
class procedure TWorldServer.LoadWorld(var character: TCharacter; var Context: TIdContext; port: integer);
var
    i: integer;
begin
    // if dead, get spawn point/map
    if (character.stats.alive = False) then
    begin
        for i := 0 to high(WorldServer.worlds) do
            if (WorldServer.worlds[i].map = character.map) then
            begin
                WorldServer.worlds[i].RandSpawnPoint(character);
                break; // avoid recursive
            end;
    end;

    // place character in new spawn
    for i := 0 to high(WorldServer.worlds) do
    begin
        WorldServer.worlds[i].Disconnect(character);
        if (WorldServer.worlds[i].map = character.map) then
        begin
            character.network.Sock := Context;
            character.network.ip := Context.Binding.PeerIP;
            character.network.port := port;
            WorldServer.worlds[i].Add(character);
        end;
    end;
end;

class procedure TWorldServer.LostMaster(Sender: TObject);
begin
    print('lost master reconnect until connected.. woot.');
end;

class procedure TWorldServer.OnConnect(Context: TIdContext);
begin
    // WorldServer.UpdateServerStatus;  now updates server status when retrieving character
    // list, which requires authentication. Only upon authentication a status update
    // will be sent.
    Context.Connection.Socket.RecvBufferSize := 512;
    Context.data := TSession.Create;
    TSession(Context.data).authenticated := False;
    TSession(Context.data).character := nil;
    TSession(Context.data).account := nil;

    // print('World Connection [' + Context.Binding.PeerIP + '].', white);
end;

class procedure TWorldServer.OnDisconnect(Context: TIdContext);
var
    i, len: integer;
begin
    try
        len := Length(WorldServer.worlds) - 1;

        if (TSession(Context.data).character <> nil) then
            for i := len downto 0 do
                WorldServer.worlds[i].Disconnect(TSession(Context.data).character^);

        WorldServer.UpdateServerStatus(True);

        { if (TSession(Context.Data).character <> NIL) then
          print('Client Disconnected [' + TSession(Context.Data).account^.user + '] @' + Context.Binding.PeerIP + ':' +
          IntToStr(Context.Binding.PeerPort), LightCyan)
          else
          print('Client disconnected [' + Context.Binding.PeerIP + '].', white); }
    except
        On E: Exception do
            print('FATAL ERROR #2151 ' + E.Message, Red);
    end;
end;

class procedure TWorldServer.pingpacket(var Context: TIdContext);
var
    packet: TPacket;
begin
    packet := TPacket.Create;
    try
        packet.types := TPackType.Ping;
        packet.action := TAction.None;
        // print('WS-Ping From [' + Context.Connection.Socket.Binding.PeerIP + '].', Cyan);
        Context.Connection.Socket.Write(packet.packetize);
    finally
        packet.Free;
    end;
end;

function TWorldServer.Online: integer;
begin
    with Server.Contexts.LockList do
        try
            Result := Count;
        finally
            Server.Contexts.UnlockList;
        end;
end;

function TWorldServer.Registered: integer;
begin
    Result := charDB.Registered();
end;

procedure TWorldServer.WorldInit;
var
    i: integer;
begin
    try
        print('Starting Worlds..');
        System_UtilExt.GetFiles('maps/', '', faDirectory);

        for i := 0 to Length(System_UtilExt.files) - 1 do
        begin
            SetLength(worlds, Length(worlds) + 1);
            worlds[high(worlds)] := TWorld.Create(System_UtilExt.files[i], i);
        end;

    except
        on E: Exception do
        begin
            print(#9 + ' ' + E.Message, System_Log.LightRed);
        end;
    end;
    print('');
end;

procedure TWorldServer.UpdateServerStatus(disconnection: boolean = False);
var
    packet: TPacket;
begin
    packet := TPacket.Create;
    cs.Acquire;
    try
        packet.types := TPackType.Serverlist;
        packet.action := TAction.Update;
        packet.sessid := WorldConf.key;
        packet.Add(WorldConf.mode);
        packet.Add(IntToStr(WorldConf.quota));
        if disconnection = True then
            packet.Add(IntToStr(Online() - 1))
        else
            packet.Add(IntToStr(Online()));
        packet.Add(IntToStr(Registered()));
        client.Socket.Write(packet.packetize);
    finally
        cs.Release;
        packet.Free;
    end;
end;

constructor TWorldServer.Create();
var
    packet: TPacket;
    threadpool: TIdSchedulerOfThreadPool;
begin
    cs := TCriticalSection.Create;
    packet := TPacket.Create;
    Server := TIdTCPServer.Create();
    client := TIdTCPClient.Create();
    charDB := TCharacterDB.Create(False);

    ReadWorldConf(WorldConf, 'conf/worldserver.conf');
    charDB.Load;

    Server.ReuseSocket := rsTrue;
    with Server.Bindings.Add do
    begin
        IPVersion := Id_IPv4;
        port := WorldConf.port;
        ip := '';
    end;

    threadpool := TIdSchedulerOfThreadPool.Create;
    threadpool.MaxThreads := 0;
    threadpool.PoolSize := 64;
    Server.Scheduler := threadpool;

    Server.OnExecute := self.OnRead;
    Server.OnDisconnect := self.OnDisconnect;
    Server.OnConnect := self.OnConnect;
    client.OnDisconnected := self.LostMaster;

{$IFDEF Win32}
    client.UseNagle := False;
{$ENDIF}
    client.port := 1596;
    client.Host := WorldConf.masterhost;
    try
        print('Querying link state..' + #9, System_Log.Brown);
        client.Connect();
    except
        print(' Fatal Error: Could not register to master server.', System_Log.Red);
        readln;
        halt;
    end;

    if client.Connected = True then
    begin
        print(' Done. [:' + IntToStr(WorldConf.port) + ']' + #10 + #13, System_Log.LightGreen, False);
        UpdateServerStatus();
        // GetAccountList(); - deprecated
        try
            print('');
            Server.Active := True;
        except
            print(' Fatal Error: Could not Bind To ' + IntToStr(WorldConf.port) + '.', System_Log.Red);
            readln;
            halt;
        end;
    end;

    SyncListener := TSyncListener.Create(client, cs);
    packet.Free;
    self.WorldInit;
end;

class procedure TWorldServer.TokenExpired(var Context: TIdContext; const sessid: string);
var
    reply: TPacket;
begin
    reply := TPacket.Create(sessid);
    try
        reply.types := TPackType.Disconnect;
        reply.action := TAction.Token;
        Context.Connection.Socket.Write(reply.packetize);
    finally
        reply.Free;
    end;
end;

class procedure TWorldServer.SendCharacters(packet: TPacket; var Context: TIdContext);
var
    reply: TPacket;
    CharacterList: TCharacterList;
    i: integer;
    sw: TStopWatch;
begin
    if AccountDB.Authenticate(packet.param(0), packet.param(1), Pointer(TSession(Context.data).account), 'World') = True
    then
    begin
        if (TSession(Context.data).authenticated = False) then // only update on NEW authentications.
            WorldServer.UpdateServerStatus(); // authenticated, now update server status.

        TSession(Context.data).authenticated := True;
        charDB.CharacterList(TSession(Context.data).account^.id, CharacterList);

        for i := 0 to high(CharacterList) do
        begin
            if (CharacterList[i] <> nil) then
            begin
                with TCharacter(CharacterList[i]^) do
                begin
                    reply := TPacket.Create;
                    reply.types := TPackType.character;
                    reply.action := TAction.Info;
                    reply.Add(stats.Name);
                    reply.Add(IntToStr(stats.level));
                    reply.Add(IntToStr(Ord(stats.profession)));

                    Context.Connection.Socket.Write(reply.packetize);
                    reply.Free;
                end;
            end;
        end;

        reply := TPacket.Create;
        reply.types := TPackType.character;
        reply.action := TAction.None;

        Context.Connection.Socket.Write(reply.packetize);
        reply.Free;
    end
    else
    begin
        reply := TPacket.Create;
        reply.types := TPackType.account;
        reply.action := TAction.Failure;
        Context.Connection.Socket.Write(reply.packetize);
        Context.Connection.Socket.Close;
        reply.Free;
    end;
end;

class procedure TWorldServer.NameCharacter(packet: TPacket; var Context: TIdContext);
var
    i, pos, port: integer;
    reply: TPacket;
    Name: string;
begin
    reply := TPacket.Create;
    try
        // create path for name already exists.

        port := StrToInt(packet.param(2));
        Name := packet.param(0);

        // add account restrictions here.. download these from the server too.
        if (Name = 'Admin') or (Name = 'Moderator') or (Name = 'GM') or (Name = 'chilimannen') or (Name = 'anjah') then
        begin
            reply.types := TPackType.Name;
            reply.action := TAction.Unavailable;

            Context.Connection.Socket.Write(reply.packetize);
        end
        else
        begin
            reply.types := TPackType.Name;
            reply.action := TAction.Success;
            Context.Connection.Socket.Write(reply.packetize);

            TSession(Context.data).character := charDB.new(TSession(Context.data).account^.id, Name,
              TProfession(StrToInt(packet.param(1))));
            TSession(Context.data).character^.sessid := packet.sessid;

            LoadWorld(TSession(Context.data).character^, Context, port);
        end;
    finally
        reply.Free;
    end;
end;

// login to local temporary account DB!
class procedure TWorldServer.LoginCharacter(packet: TPacket; var Context: TIdContext);
var
    port: integer;
    prof: TProfession;
    reply: TPacket;
begin
    reply := TPacket.Create;
    try
        prof := TProfession(StrToInt(packet.param(0)));
        port := StrToInt(packet.param(1));

        if (TSession(Context.data).authenticated = True) then
        begin
            if charDB.character(TSession(Context.data).account^.id, prof, Pointer(TSession(Context.data).character)) = True
            then
            begin
                TSession(Context.data).character^.sessid := packet.sessid;
                WorldServer.LoadWorld(TSession(Context.data).character^, Context, port);

                print('Account [' + TSession(Context.data).account^.user + '] Joined [' + TSession(Context.data)
                  .character^.map + '] With Character [' + TSession(Context.data).character^.stats.Name + '].',
                  LightCyan);
            end
            else
            begin
                reply.types := TPackType.Name;
                reply.action := TAction.None;
                Context.Connection.Socket.Write(reply.packetize);
            end;
        end;
    finally
        reply.Free;
    end;
end;

class procedure TWorldServer.OnRead(AContext: TIdContext);
var
    packet: TPacket;
    data: TIdBytes;
    len: word;
begin
    packet := TPacket.Create;

    try
        packet.ip := AContext.Connection.Socket.Binding.PeerIP;
        packet.port := AContext.Connection.Socket.Binding.PeerPort;

        // get message length
        AContext.Connection.Socket.ReadBytes(data, 2);
        Move(data[0], len, SizeOf(len));

        // verify packet size.
        if (len > Conf_Protocol.MAXBYTES) or (len < 0) then
        begin
            // drop all bytes in buffer.
            AContext.Connection.Socket.InputBuffer.Clear;
            // AContext.Connection.Socket.DiscardAll;
        end
        else
        begin

            // reset buffer
            SetLength(data, 0);

            // get message
            AContext.Connection.Socket.ReadBytes(data, len);

            // parse and unpack
            packet.unpacketize(BytesToString(data, IndyTextEncoding(TPacket.Encoding)));

            if (TokenDB.Verify(packet.sessid, packet.ip, packet.port).valid) then
            begin
                // login block -------------------------------------
                case (packet.types) of
                    TPackType.character:
                        if (packet.action = TAction.Info) then
                            SendCharacters(packet, AContext);
                    TPackType.Ping:
                        pingpacket(AContext);
                end;

                // authenticated block -----------------------------
                if TSession(AContext.data).authenticated = True then
                begin
                    case (packet.types) of
                        TPackType.Name:
                            NameCharacter(packet, AContext);
                        TPackType.character:
                            if (packet.action = TAction.Select) then
                                LoginCharacter(packet, AContext);
                    end;
                end;
            end
            else
                TokenExpired(AContext, packet.sessid);
        end;

    finally
        packet.Free;
    end;
end;

procedure Initialize;
begin
    WorldServer := TWorldServer.Create();
end;

end.
