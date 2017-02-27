unit Network_WorldServer;

// tcp server in passive mode.
// used for game/character select.
{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, Dialogs, idSocketHandle,
    IdTCPClient, Graphics, {$IFDEF Win32}Windows, ShellApi, VCL.controls, WinSock, {$ENDIF}
    Conf_Protocol,
    System_UtilExt, Engine_Map, Effects_Blending, SyncObjs, Debug_Stopwatch,
    System_ping, IdGlobal;

type
    TConnection = class(TThread)
    protected
        procedure Execute; override;
    private
        cs: TCriticalSection;
    public
        client: TIdTCPClient;
        constructor Create(host: string; port: integer);
        procedure AuthFailure();
        procedure SendName();
        procedure CharacterData(var packet: TPacket);
        procedure CharacterNull();
        procedure CharacterName(var packet: TPacket);
        procedure LoginCharacter(prof: TProfession);
        procedure RequestCharacters();
        procedure PingResponse;
        procedure TokenExpired();
        procedure Ping();
        procedure LoadMap(var packet: TPacket);
        procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
        class procedure disconnected(Sender: TObject);
    end;

procedure initialize();

var
    WorldCon: TConnection; // worldconnection
    TPing: TPingData;

implementation

uses System_Initializer, GUI_LoginScreen, GUI_ServerSelect, GUI_CharSelect,
    Main,
    AsphyreTypes, Vectors2px, Engine_Player, Network_World, System_Log,
    Network_MetaServer, GUI_StatusBox, GUI_Bars, GUI_Chat, GUI_SkillBar,
    System_Camera, System_Multiplayer, GUI_Banner, System_Animation, System_DayTime, System_Audio, Effects_Smoke;

procedure TConnection.RequestCharacters();
var
    packet: TPacket;
begin
    try
        cs.Acquire;
        packet := TPacket.Create;
        try
            packet.types := Tpacktype.Character;
            packet.action := TAction.Info;
            packet.Add(LoginScreen.textfield.text);
            packet.Add(LoginScreen.passfield.text);
            client.socket.Write(packet.packetize);
        finally
            packet.free;
            cs.Release;
        end;
    except
        self.disconnected(NIL);
    end;
end;

procedure initialize;
begin
    TPing := TPingData.Create;
end;

procedure TConnection.Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
    ListOfStrings.Clear;
    ListOfStrings.Delimiter := Delimiter;
    ListOfStrings.DelimitedText := Str;
end;

class procedure TConnection.disconnected(Sender: TObject);
begin
    if (Main.View = TView.TGameInterface) or (Main.View = TView.TCharacterSelect) then
    begin
        print('WorldServer lost connection.', red);
        Main.View := Main.TView.TServerSelect;

        StatusBox.settext('Disconnected From Server.');
        StatusBox.show;
        StatusBox.finished;
    end;
end;

// 1. Check with master, 2. Check if exist then loadpacket+data. not exist create and ask
// for name.
procedure TConnection.LoginCharacter(prof: TProfession);
var
    packet: TPacket;
begin
    packet := TPacket.Create;
    cs.Acquire;
    try
        packet.types := Tpacktype.Character;
        packet.action := TAction.Select;
        packet.Add(inttostr(ord(prof)));
        packet.Add(inttostr(WorldConnection.port));
        try
            client.socket.Write(packet.packetize);
        except
            self.disconnected(NIL);
        end;
    finally
        cs.Release;
    end;
end;

procedure TConnection.TokenExpired();
begin
    if (Main.View = TView.TGameInterface) or (Main.View = TView.TCharacterSelect) then
    begin
        print('Session Expired!.', red);
        Main.View := Main.TView.TLogin;

        StatusBox.settext('Session Expired.\Please Login Again.');
        StatusBox.show;
        StatusBox.finished;
    end;
end;

procedure TConnection.SendName();
var
    packet: TPacket;
begin
    packet := TPacket.Create;
    cs.Acquire;
    try
        StatusBox.field.hide;
        StatusBox.button.hide;
        StatusBox.show;
        StatusBox.settext('Checking Name ..');
        packet.types := Tpacktype.name;
        packet.action := TAction.Update;
        packet.Add(StatusBox.field.text);
        packet.Add(inttostr(ord(player.profession)));
        packet.Add(inttostr(WorldConnection.port));
        WorldCon.client.socket.Write(packet.packetize);
    except
        WorldCon.disconnected(NIL);
    end;
    cs.Release;
    packet.free;
end;

procedure TConnection.CharacterName(var packet: TPacket);
begin
    case TAction(packet.action) of
        TAction.exist:
            begin
                StatusBox.settext('Name Taken.');
                StatusBox.show;
                StatusBox.finished(SendName);
                StatusBox.field.show;
                StatusBox.field.show;
            end;
        TAction.unavailable:
            begin
                StatusBox.settext('Name Not Available.');
                StatusBox.show;
                StatusBox.finished(SendName);
                StatusBox.field.show;
                StatusBox.field.show;
            end;
        TAction.success:
            begin
                StatusBox.settext('Character Created!');
                WorldCon.LoginCharacter(player.profession);
            end;
        TAction.None:
            begin
                StatusBox.show;
                StatusBox.finished(SendName);
                StatusBox.settext('Please Enter A Name For Your Character.');
                StatusBox.field.show;
                StatusBox.field.focus := true;
            end;
    end;
end;

procedure TConnection.CharacterNull();
begin
    StatusBox.hide;
end;

procedure TConnection.CharacterData(var packet: TPacket);
begin
    CharSelect.AddCharacter(packet.param(0), packet.param(1), TProfession(StrToInt(packet.param(2))));
end;

constructor TConnection.Create(host: string; port: integer);
begin
    inherited Create(false);
    cs := TCriticalSection.Create;
    client := TIdTCPClient.Create();
    client.OnDisconnected := self.disconnected;
    client.port := port;
    client.host := host;
{$IFDEF Win32}
    client.UseNagle := false;
{$ENDIF}
    client.ConnectTimeout := 1000;
    client.ReadTimeout := 1000;
end;

procedure TConnection.Ping();
var
    packet: TPacket;
begin
    packet := TPacket.Create;
    cs.Acquire;
    try
        packet.types := Tpacktype.Ping;
        packet.action := TAction.None;
        TPing.pingquery;
        WorldCon.client.socket.Write(packet.packetize);
    finally
        cs.Release;
        packet.free;
    end;
end;

procedure TConnection.LoadMap(var packet: TPacket);
var
    map: string;
    port: integer;
    x, y: single;
begin
    try
        map := packet.param(0);
        port := StrToInt(packet.param(1));
        x := StrToInt(packet.param(2));
        y := StrToInt(packet.param(3));
        player.name := packet.param(4);
        player.level := StrToInt(packet.param(5));
        player.speed := SafeFloat(packet.param(6));
        player.aid := StrToInt(packet.param(7));
        daytime.Secs(StrToInt(packet.param(8)));

        multiplayer.Clear;

        StatusBox.settext('Loading Map..');
        MapMan.Load(map);

        StatusBox.settext('Loading Sound..');
        SoundMaster.Play(map, true);

        StatusBox.settext('Loading Data..');
        GUIBar.character_name.text := player.name;
        GUIBar.character_level.text := 'lv.' + inttostr(player.level);

        player.x := x;
        player.y := y;
        player.alive := true;
        player.sprite.animate(eAnim.aStanding);

        cam.focus(trunc(x - initializer.Swidth / 2), trunc(y - initializer.Sheight / 2));

        // load media here..
        FileMan.datasize := 0; // new payload
        WorldConnection.setport(port);

        if (FileMan.gamedata = false) then
        begin
            FileMan.FileSearch(root + 'media/game/', '*.png', true);
            FileMan.Load();
            FileMan.gamedata := true;
        end;

        SkillBar.Refresh;

        Main.View := TView.TGameInterface;

        SmokeMan.Add(player.getx, player.gety, 10, $FFFFFFFF);
        GUIBanner.TextFromFile('data/banner_game.txt', 720);
        sleep(16); // wait for frame to draw

        // RenderForm.closeview;  #enable for splash suport
    except
        on E: Exception do
        begin
            StatusBox.settext(E.Message);
            StatusBox.finished(StatusBox.Terminate);
            sleep(3000);
{$IFDEF Win32}
            ExitProcess(0);
{$ELSE}
            Halt;
{$ENDIF}
        end;
    end;
    StatusBox.hide;
end;

procedure TConnection.PingResponse;
begin
    TPing.pingreply;
end;

procedure TConnection.AuthFailure();
begin
    StatusBox.show;
    StatusBox.settext('Authentication Failure.');
    StatusBox.finished;
    LoginScreen.SetView;
end;

// this is the master server.
procedure TConnection.Execute;
var
    packet: TPacket;
    data: TIdBytes;
    len: word;
begin
    self.FreeOnTerminate := true;
    // StatusBox.show;
    // StatusBox.settext('Connecting..');

    try
        client.Connect;
    except
        StatusBox.show;
        StatusBox.settext('Could Not Connect.');
        StatusBox.finished;

        Main.View := Main.TView.TServerSelect;
    end;

    if client.connected then
    begin
        MetaCon.Disconnect;
        RequestCharacters();

        try
            while (client.connected) do
            begin
                sleep(1);

                cs.Acquire;
                while client.socket.InputBuffer.Size > 0 do
                begin
                    packet := TPacket.Create;

                    // get message length
                    client.socket.ReadBytes(data, 2);
                    Move(data[0], len, SizeOf(len));

                    // verify packet size.
                    if (len > Conf_Protocol.MAXBYTES) or (len < 0) then
                    begin
                        // drop all bytes in buffer.
                        client.socket.InputBuffer.Clear;
                        client.socket.DiscardAll;
                        SetLength(data, 0);
                    end
                    else
                    begin
                        // reset buffer
                        SetLength(data, 0);

                        // get message
                        client.socket.ReadBytes(data, len);

                        // parse and unpack
                        packet.unpacketize(BytesToString(data, IndyTextEncoding(packet.encoding)));
                        SetLength(data, 0);

                        case (packet.types) of
                            Tpacktype.Disconnect:
                                if (packet.action = TAction.Token) then
                                    TokenExpired();
                            Tpacktype.account:
                                if (packet.action = TAction.Failure) then
                                    AuthFailure();
                            Tpacktype.Character:
                                case (packet.action) of
                                    TAction.Info:
                                        CharacterData(packet);
                                    TAction.None:
                                        CharacterNull;
                                end;
                            Tpacktype.name:
                                CharacterName(packet);
                            Tpacktype.map:
                                if packet.action = TAction.Load then
                                    LoadMap(packet);
                            Tpacktype.Ping:
                                PingResponse;
                        end;
                    end;
                    packet.free;
                end;
                cs.Release;
            end;

        except
            Main.View := Main.TView.TServerSelect;
            MetaCon.ListServers;
            StatusBox.show;
            StatusBox.settext('Disconnected From Server.');
            StatusBox.finished;
        end;
    end
    else
    begin
        ServerScreen.SetView;
        StatusBox.show;
        StatusBox.settext('Connection Failure.');
        StatusBox.finished;
    end;
end;

end.