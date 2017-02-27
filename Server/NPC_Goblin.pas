unit NPC_Goblin;

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses Engine_NPC, SysUtils, Engine_Tick, System_Spells, IdUDPServer, Engine_Characters;

// implements process and interact as abstract.
// must implement npc_networking, which uses UDP to transmit movement.
// quest/trade data goes through TCP.

// on startup / shutdown store the current npc in npc.dat
// on startup / add npcs from npclist

// NPC engine is invoked from every worldserver.

type
    TGoblin = class(TNPC)
    private
        target: ^TCharacter;
    public
        procedure process(SpellMan: TSpellMan; chars: TPlayerList; socket: TIdUDPServer); override;
        procedure interact(); override;
        constructor create();
    end;

implementation

uses System_Log, Conf_Protocol, Math;

// create the gobling.
constructor TGoblin.create;
begin
    inherited create;
    stats.name := 'Murderous Goblin';
    stats.alive := true;
    world.entityId := 1000 + random(9999);

    world.X := 2250 + random(800) - 400;
    world.Y := 2250 + random(800) - 400;
    world.waypoint.X := trunc(world.X);
    world.waypoint.Y := trunc(world.Y);


    stats.profession := TProfession(random(3));

    stats.level := 1 + random(9);
    charDB.LevelUp(stats, state);

    stats.INT := 1;
    stats.STR := 1;
    stats.CON := 20;

    target := nil;

    charDB.CalculateStats(stats, state);
    stats.hp := stats.hpmax div 2;
    stats.energy := stats.energymax div 2;
    world.movementtype := TMovementType.stopped;
end;

// move/AI/Attack - update the state to every client on map (done through the other stuffs ?)
procedure TGoblin.process(SpellMan: TSpellMan; chars: TPlayerList; socket: TIdUDPServer);
var
    spell: TSpell;
    i: integer;
    delta: integer;
begin
    // if health = 0 then npcman.remove(self);

    if stats.alive then
    begin
        delta := -1;
        target := nil;
        // get target.

        for i := 0 to high(chars) do
        begin
            if delta < (abs(chars[i].world.X - world.X) + abs(chars[i].world.Y - world.Y)) then
            begin
                delta := trunc(abs(chars[i].world.X - world.X) + abs(chars[i].world.Y - world.Y));
                target := @chars[i]^;
            end;
        end;

        if (delta < 400) and (target <> NIL) then
        begin

           // spell := SpellMan.getSpell(2, stats.profession);

            // can mod by movement for better precision
            SpellMan.Fire(ord(stats.profession), @stats, @world,
              DegToRad(RadToDeg(Math.ArcTan2(world.Y - target^.world.Y, world.X - target^.world.X)) + 180));

            // if delta < maxrange moveclose if not inrange
            // if delta > maxrange, drop and move back
        end
        else if (delta > 400) and (target <> NIL) then
        begin

        end;

        if (random(10) > 3) then
        begin
            if (target <> nil) then

                self.move(TMovementType.waypoint, 0, trunc(target^.world.X + random(75)),
                  trunc(target^.world.Y + random(75)), chars, socket);
        end;
    end;
end;

// interact with the goblin? waaat
procedure TGoblin.interact;
begin
end;

end.