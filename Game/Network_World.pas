unit Network_World;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses IdUDPServer, IdGlobal, IdSocketHandle, SysUtils,
    System_UtilExt, classes, System_Multiplayer, {$IFDEF Win32}Windows, {$ENDIF} Conf_Protocol, Dialogs,
    Engine_Player, SyncObjs, System_Ping;

type
    TWorldConnection = class
    private
        procedure updatePlayerPacket(var packet: TPacket);
    public
        cs: TCriticalSection;
        server: TIdUDPServer;
        port, remport: integer;
        host: string[25];
        procedure Attribpacket(var packet: TPacket);
        procedure SendMessage(sender, receiver, text: string; x, y: integer);
        procedure udpread(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); Register;
        procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
        procedure UpdateMovement(dir, x, y: integer; MovementType: TMovementType);
        procedure DisconnectPacket(var packet: TPacket);
        procedure SpellPacket(var packet: TPacket);
        procedure HitPacket(var packet: TPacket);
        procedure sethost(ip: string);
        procedure setport(port: integer);
        procedure pingresponse();
        procedure deathpacket(var packet: TPacket);
        procedure spawnPlayerPacket(var packet: TPacket; onConnect: boolean = false);
        procedure ping();
        procedure CastSpell(id: integer; dir: single);
        procedure Respawn();
        procedure SendDisconnect();
        procedure TokenExpired();
        constructor create();
    end;

procedure Initialize;

var
    WorldConnection: TWorldConnection;
    UPing: TPingData;

implementation

uses System_Initializer, System_Log, GUI_Chat, System_Text, GUI_StatusBox,
    Engine_Projectiles, System_Spells,
    Effects_Splat{$IFDEF Win32}, System_Audio{$ENDIF}, Engine_Map, GUI_Targeting, GUI_Inventory, Main;

procedure TWorldConnection.SendDisconnect();
var
    packet: TPacket;
begin
    packet := TPacket.create();
    try
        packet.types := TPackType.Disconnect;
        packet.action := TAction.none;
        cs.Acquire;
        server.SendBuffer(host, remport, packet.packetize);
        cs.Release;
    finally
        packet.free;
    end;
end;

procedure TWorldConnection.TokenExpired();
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

constructor TWorldConnection.create();
begin
    cs := TCriticalSection.create;
    server := TIdUDPServer.create;
    server.ThreadedEvent := true;
    host := '127.0.0.1';

    with server.Bindings.add do
    begin
        ip := '';
        port := 0;
    end;

    port := server.Binding.port;
    server.OnUDPRead := self.udpread;
    server.Active := true;
    print('Listening UDP on port: #' + inttostr(port));
end;

procedure TWorldConnection.Respawn;
var
    packet: TPacket;
begin
    StatusBox.settext('Loading..');
    StatusBox.button.Hide;
    packet := TPacket.create;
    packet.types := TPackType.character;
    packet.action := TAction.Respawn;
    cs.Acquire;
    try
        server.SendBuffer(host, remport, packet.packetize);
    finally
        cs.Release;
    end;
end;

procedure TWorldConnection.ping;
var
    packet: TPacket;
begin
    packet := TPacket.create;
    cs.Acquire;
    try
        packet.types := TPackType.ping;
        packet.action := TAction.none;
        UPing.PingQuery;
        server.SendBuffer(host, remport, packet.packetize);
    finally
        packet.free;
        cs.Release;
    end;
end;

procedure TWorldConnection.SpellPacket(var packet: TPacket);
var
    senderptr: Pointer;
    sender: TNetPlayer;
    spell: TSpell;
    dir: single;
    spellID, prjId, aID: integer;
begin
    aID := StrToInt(packet.param(0));
    spellID := StrToInt(packet.param(1));
    dir := SafeFloat(packet.param(2));
    prjId := StrToInt(packet.param(3));

    sender := Multiplayer.getplayer(aID);
    if (sender.aID <> -1) then
    begin
        if (SpellMan.getSpell(spellID, spell)) then
            ProjectileMan.fire(sender.x, sender.y, spell.speed, dir, spell.ttl, spell.sprite, $FFFFFFFF, prjId);
    end
    else if (player.aID = aID) and (SpellMan.getSpell(spellID, spell)) then
        ProjectileMan.fire(player.x, player.y, spell.speed, dir, spell.ttl, spell.sprite, $FFFFFFFF, prjId);

{$IFDEF Win32}
    SoundMaster.Play('spell_1'); // spell sound from spell.
{$ENDIF}
end;

procedure TWorldConnection.CastSpell(id: integer; dir: single);
var
    packet: TPacket;
begin
    packet := TPacket.create;
    cs.Acquire;
    try
        packet.types := TPackType.character;
        packet.action := TAction.spell;
        packet.add(inttostr(id));
        packet.add(floattostr(dir));
        server.SendBuffer(host, remport, packet.packetize);
    finally
        cs.Release;
        packet.free;
    end;
end;

procedure TWorldConnection.SendMessage(sender, receiver, text: string; x: integer; y: integer);
var
    packet: TPacket;
begin
    packet := TPacket.create;
    cs.Acquire;
    try
        packet.types := TPackType.Chat;
        packet.action := TAction.msg;
        packet.add(sender);
        packet.add(receiver);
        packet.add(text);
        packet.add(inttostr(x));
        packet.add(inttostr(y));
        server.SendBuffer(host, remport, packet.packetize);
    finally
        packet.free;
        cs.Release;
    end;
end;

procedure TWorldConnection.Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
    ListOfStrings.Clear;
    ListOfStrings.Delimiter := Delimiter;
    ListOfStrings.DelimitedText := Str;
end;

procedure TWorldConnection.UpdateMovement(dir, x, y: integer; MovementType: TMovementType);
var
    packet: TPacket;
begin
    packet := TPacket.create;
    try
        packet.types := TPackType.character;
        packet.action := TAction.Movement;
        packet.add(inttostr(ord(MovementType)));
        packet.add(inttostr(dir));
        packet.add(inttostr(x));
        packet.add(inttostr(y));

        cs.Acquire;
        server.SendBuffer(host, remport, packet.packetize);
        cs.Release;
    finally
        packet.free;
    end;
end;

// directed single-player update.
procedure TWorldConnection.Attribpacket(var packet: TPacket);
var
    xpgain, levelgain: integer;
begin
    xpgain := StrToInt(packet.param(4)) - round(player.experience);
    levelgain := StrToInt(packet.param(6)) - player.level;

    player.health := StrToInt(packet.param(0));
    player.maxhealth := StrToInt(packet.param(1));
    player.energy := StrToInt(packet.param(2));
    player.maxenergy := StrToInt(packet.param(3));
    player.experience := StrToInt(packet.param(4));
    player.maxexperience := StrToInt(packet.param(5));
    player.level := StrToInt(packet.param(6));
    player.spellpower := StrToInt(packet.param(7));
    player.attackpower := StrToInt(packet.param(8));
    player.skillpts := StrToInt(packet.param(9));
    player.statpts := StrToInt(packet.param(10));

    if (xpgain > 0) then
        ChatEngine.addMessage('', 'Gained ' + inttostr(xpgain) + ' Experience Points.', $FF00FFFF);

    if (levelgain > 0) then
        ChatEngine.addMessage('', 'Reached Level ' + inttostr(player.level) + ', Congratulations!', $FF00FFFF);

    GUIInventory.Refresh;
end;

// sender-id, target-id, value, color
procedure TWorldConnection.HitPacket(var packet: TPacket);
var
    target, sender, prjId: integer;
    mplayer: TNetPlayer;
begin
    try
        target := StrToInt(packet.param(0));
        sender := StrToInt(packet.param(1));
        prjId := StrToInt(packet.param(4));

        ProjectileMan.RemoveId(prjId);

        mplayer := Multiplayer.getplayer(target);
        if (mplayer.aID <> -1) then
            TextEngine.add(mplayer.x, mplayer.y, packet.param(2), StrToInt64(packet.param(3)))
        else if (player.aID = target) then
        begin
            TextEngine.add(player.x, player.y, packet.param(2), StrToInt64(packet.param(3)));
            SplatMan.splat(BloodSplat);
            player.health := player.health + StrToInt(packet.param(2));
        end;
    finally
        // some formating error.. disregard.
    end;
end;

// aID, text, colr
procedure TWorldConnection.deathpacket(var packet: TPacket);
var
    aID: integer;
begin
    aID := StrToInt(packet.param(0));

    ChatEngine.addMessage('', packet.param(1), StrToInt64(packet.param(2)));

    if (player.aID = aID) then
        player.die;

    GUITargetFrame.clearTarget(aID);
    Multiplayer.die(aID);
end;

procedure TWorldConnection.pingresponse();
begin
    UPing.PingReply;
end;

procedure TWorldConnection.DisconnectPacket(var packet: TPacket);
begin
    try
        GUITargetFrame.clearTarget(StrToInt(packet.param(0)));
        Multiplayer.Remove(StrToInt(packet.param(0)));
    except
        //
    end;
end;

procedure TWorldConnection.updatePlayerPacket(var packet: TPacket);
var
    aID, level, hp, hpmax, mp, mpmax: integer;
begin
    aID := StrToInt(packet.param(0));
    level := StrToInt(packet.param(1));
    hp := StrToInt(packet.param(2));
    hpmax := StrToInt(packet.param(3));
    mp := StrToInt(packet.param(4));
    mpmax := StrToInt(packet.param(5));
    Multiplayer.UpdatePlayer(aID, level, hp, hpmax, mp, mpmax);
end;

// disables message
procedure TWorldConnection.spawnPlayerPacket(var packet: TPacket; onConnect: boolean = false);
var
    aID, x, y, level, hp, hpmax, mp, mpmax: integer;
    name: String;
    profession: Conf_Protocol.TProfession;
begin
    aID := StrToInt(packet.param(0));
    x := StrToInt(packet.param(1));
    y := StrToInt(packet.param(2));

    case (StrToInt(packet.param(3))) of
        0:
            profession := TProfession.Mage;
        1:
            profession := TProfession.Warrior;
        2:
            profession := TProfession.Thief;
    end;

    name := packet.param(4);
    level := StrToInt(packet.param(5));
    hp := StrToInt(packet.param(6));
    hpmax := StrToInt(packet.param(7));
    mp := StrToInt(packet.param(8));
    mpmax := StrToInt(packet.param(9));

    Multiplayer.SpawnPlayer(aID, x, y, name, level, hp, hpmax, mp, mpmax, profession);

    if not(onConnect) then
        ChatEngine.addMessage('', packet.param(4) + ' Has Joined ' + MapMan.Image.name + '.', $FF00FF00);
end;

// packet switching to multiplayer handler.
procedure TWorldConnection.udpread(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
    i: integer;
    packet: TPacket;
    data: TIdBytes;
    len: word;
begin
    packet := TPacket.create;
    try

        if (length(AData) > 2) and (length(AData) < MAXBYTES) then
        begin
            SetLength(Data, length(AData) - 2);
            Move(AData[2], Data[0], length(AData) - 2);

            packet.ip := ABinding.PeerIP;
            packet.port := ABinding.PeerPort;
            packet.unpacketize(BytesToString(Data, IndyTextEncoding(packet.Encoding())));

            case (packet.types) of
                World:
                    begin
                        if (packet.action = TAction.Death) then
                            deathpacket(packet);
                        if (packet.action = TAction.collision) then
                            HitPacket(packet);
                        if (packet.action = TAction.spell) then
                            SpellPacket(packet);
                    end;
                Disconnect:
                    if packet.action = TAction.Token then
                        TokenExpired()
                    else
                        DisconnectPacket(packet);
                character:
                    begin
                        case (packet.action) of
                            Update:
                                updatePlayerPacket(packet);
                            Online:
                                spawnPlayerPacket(packet, true);
                            Spawn:
                                spawnPlayerPacket(packet);
                            Movement:
                                begin
                                    try
                                        Multiplayer.Movepacket(packet);
                                    except
                                        on E: Exception do
                                        print(E.Message);
                                    end;
                                end;
                            attribute:
                                Attribpacket(packet);
                        end;
                    end;
                TPackType.Chat:
                    ChatEngine.add(packet);
                TPackType.ping:
                    pingresponse();
            end;
        end;
    finally
        packet.free;
    end;
end;

procedure TWorldConnection.sethost(ip: string);
begin
    host := ip;
    print('host set:' + ip);
    ping;
end;

procedure TWorldConnection.setport(port: integer);
var
    packet: TPacket;
begin
    remport := port;
    print('port set:' + inttostr(port));
    ping;
end;

procedure Initialize;
begin
    WorldConnection := TWorldConnection.create();
    UPing := TPingData.create;
end;

end.