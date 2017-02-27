unit Engine_NPC;

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}
// spawn a thread to manage the lifecycle of npcs, spawning, etc.
// logic is overriden abstract

// type loot-table
interface

uses SyncObjs, Engine_Characters, Classes, SysUtils, System_Spells, Conf_Protocol, IdGlobal, IdUDPClient, IdUDPServer;

const
    NPCTICKDIFF = 20; // tick every X tps

type
    TNPC = class
    protected
        // ID: Integer;
    public
        world: TEntityWorld;
        stats: TEntityStats;
        state: TEntityState;

        // call the logic
        procedure process(SpellMan: TSpellMan; chars: TPlayerList; socket: TIdUDPServer); virtual; abstract;
        procedure move(movementtype: TMovementType; dir: integer; x: integer; y: integer; chars: TPlayerList;
          socket: TIdUDPServer);

        // define what happens on player interaction, throw dialogs inhere etc?
        procedure interact(); virtual; abstract; // spawn a dialog.
    end;

type
    TNPCList = array of TNPC;

type
    TNPCMan = class
    protected
    private
        cs: TCriticalSection;
        fList: TNPCList;
        tick: integer;
        procedure remove(npc: TNPC);
        function getList(): Pointer;
    public
        // spawn a new npc, notify network!
        procedure add(npc: TNPC);

        // return the array of npcs
        property list: Pointer read getList;

        // update the logic
        procedure process(chars: TPlayerList; TPS: integer; var SpellMan: TSpellMan; socket: TIdUDPServer);

        // constructor
        constructor create(map: string);
    end;

implementation

uses NPC_Goblin, System_Log, Network_World;

function TNPCMan.getList;
begin
    cs.Acquire;
    try
        result := @fList;
    finally
        cs.Release;
    end;
end;

procedure TNPC.move(movementtype: TMovementType; dir: integer; x: integer; y: integer; chars: TPlayerList; socket: TIdUDPServer);
var
    j: integer;
    update: TPacket;
begin
    world.movementtype := movementtype;
    try

        for j := 0 to high(chars) do
        begin
            update := TPacket.create;
            update.sessid := chars[j]^.sessid;
            update.types := TPackType.character;
            update.action := TAction.MOVEMENT;
            update.add(IntToStr(ord(TPlayerType.Multiplayer)));
            update.add(IntToStr(ord(movementtype)));
            update.add(IntToStr(world.EntityId));
            update.add(IntToStr(world.dir));

            if (movementtype = TMovementType.waypoint) then
            begin
                world.waypoint.x := x;
                world.waypoint.y := y;
                update.add(IntToStr(world.waypoint.x));
                update.add(IntToStr(world.waypoint.y));
            end
            else if (movementtype = TMovementType.direction) then
            begin
                world.dir := dir;
                update.add(IntToStr(trunc(world.x)));
                update.add(IntToStr(trunc(world.y)));
            end
            else if (movementtype = TMovementType.teleport) then
            begin
                world.x := x; // teleport, only acessible by npcs :) !
                world.y := y;
                update.add(IntToStr(trunc(world.x)));
                update.add(IntToStr(trunc(world.y)));
            end;

            update.add(FloatToStr(stats.speed));

            socket.SendBuffer(chars[j]^.network.ip, chars[j]^.network.port, update.packetize { , packet.Encoding } );
            update.free;
        end;
    finally
    end;
end;

// goblin can send data, but should never be the recipient!
constructor TNPCMan.create(map: string);
var
    i: integer;
begin
    cs := TCriticalSection.create();
    setlength(fList, 0);

    // read settings from npc in /map/npc.dat and create then
    if (map = 'Warrior Castle') then
    begin
        for i := 0 to 100 do
        begin
            setlength(fList, length(fList) + 1);
            fList[i] := TGoblin.create;
        end;
    end;

    tick := 0;
end;

procedure TNPCMan.process(chars: TPlayerList; TPS: integer; var SpellMan: TSpellMan; socket: TIdUDPServer);
var
    len, i, j: integer;
begin
    inc(tick);

    if (tick = NPCTICKDIFF) then
    begin
        tick := 0;
        cs.Acquire;
        try
            len := length(fList);
            for i := 0 to len - 1 do
            begin
                fList[i].process(SpellMan, chars, socket);
            end;
        finally
            cs.Release;
        end;
    end;
end;

procedure TNPCMan.remove(npc: TNPC);
var
    i, j: integer;
    found: boolean;
begin
    found := false;
    for i := 0 to high(fList) do
        if (@npc = @fList[i]) then
            for j := i to high(fList) - 1 do
            begin
                fList[j] := fList[j + 1];
                found := true;
            end;

    if (found) then
        setlength(fList, length(fList) - 1);
end;

procedure TNPCMan.add(npc: TNPC);
var
    len: integer;
begin

    cs.Acquire;
    try
        len := length(fList);
        setlength(fList, length(fList) + 1);
        fList[len] := npc;
    finally
        cs.Release;
    end;
end;

end.
