unit System_Spells;

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}
// need speed and dir and location of the other players (npcs and monsters too).
// need support for instant aoe spells.
// implementes TSpell.

uses SysUtils, Conf_Protocol, System_Log, SyncObjs, Math,
  classes, Debug_StopWatch, Engine_Characters, System_Cooldown;

type
  TPoint = record
    x, y: integer;
  end;

type
  TTargetType = (Projectile = 0, Area = 1, Explosion = 2, Self = 3, Cone = 4);
  // used for line of missile

type
  TSpellType = (WARRIOR_BASIC = 0, THIEF_BASIC = 1, MAGE_BASIC = 2, EMPTY = -1);

type
  TDamageType = (Physical, Magical, Poison, Full, Healing);

  // data for each level of the spell
  // a spell
type
  TSpell = record
  public
    SpellType: TSpellType;
    name: string[16];
    sprite: TPoint;
    targeting: TTargetType;
    cost, casttime, ttl, timer: integer;
    learned: boolean;
    damage, speed, duration, Cooldown: single;
    damagetype: TDamageType;
    // cooldown time for reset, timer time left of reset
    description: string[60];
  end;

  // organize the spells into trees
type
  TSpellTree = record
    name: string[16];
    spells: array [0 .. 20] of TSpell;
  end;

type
  TSpellProfession = record
  public
    trees: array [0 .. 2] of TSpellTree;
    profession: TProfession;
    basic: TSpell; // basic attacks are not a part of a tree
  end;

type
  THitData = record
  public
    target, color, projectileId: integer;
    sender_stats, sender_world: pointer;
    value: integer;
    spellname, targetname: string[16];
    fatality, collision: boolean;
  end;

type
  THitList = array of THitData;

type
  TProjectile = Record
  public
    x, y, dir: single;
    senderworld, senderstats: pointer;
    ttl, id: integer;
    spell: TSpell;
  End;

type
  TCastAction = Record
  public
    success: boolean;
    id: integer;
  end;

type
  TSpellMan = class
  private
    cooldowns: TCooldownMan;
    prj: array of TProjectile;
    cs: TCriticalSection;
    SpellCount: integer;
    SpellSet: array of TSpellProfession;
    procedure Movement();
    procedure Orphan();
    Function collision(var world: TEntityWorld; var stats: TEntityStats): THitData;
  public
    function CollisionPlayer(players: TPlayerList): THitList;
    function CollisionNPC(npc: pointer): THitList;

    function Fire(spellId: integer; senderstats, senderworld: pointer; dir: single): TCastAction;
    function getSpell(id: integer; profession: TProfession): TSpell;
    procedure Process(tps: integer);
    constructor create;
  end;

  // procedure initialize(); read spell file.

implementation

uses Network_World, Conf_Server, Engine_NPC;

function TSpellMan.getSpell(id: integer; profession: TProfession): TSpell;
var
  i, j: integer;
begin
  result.SpellType := TSpellType.EMPTY;

  if (integer(SpellSet[ord(profession)].basic.SpellType) = id) then
  begin
    result := SpellSet[ord(profession)].basic;
  end
  else
    for i := 0 to high(SpellSet[ord(profession)].trees) do
      for j := 0 to high(SpellSet[ord(profession)].trees[i].spells) do
        if (integer(SpellSet[ord(profession)].trees[i].spells[j].SpellType) = id) then
        begin
          result := SpellSet[ord(profession)].trees[i].spells[j];
        end;
end;

procedure TSpellMan.Orphan;
var
  i, j, trail, len, origlen: integer;
begin
  len := length(prj);

  for i := 0 to len - 1 do
  begin
    if prj[i].ttl < 1 then
    begin
      trail := len - i - 1;
      if trail > 0 then
        Move(prj[i + 1], prj[i], SizeOf(TProjectile) * (trail));
      Dec(len);
    end;
  end;

  setlength(prj, len);
end;

// if spell is instant = use targetX/targetY..
function TSpellMan.Fire(spellId: integer; senderstats, senderworld: pointer; dir: single): TCastAction;
var
  Projectile: TProjectile;
  spell: TSpell;
  len: integer;
begin
  result.success := false;

  with TEntityStats(senderstats^) do
  begin
    spell := Self.getSpell(spellId, TEntityStats(senderstats^).profession);
    if (alive) and (spell.SpellType <> TSpellType.EMPTY) then
    begin
      if (cooldowns.mayfire(senderstats, ord(spell.SpellType))) and (energy > spell.cost) then
      begin
        cooldowns.Add(senderstats, spell.Cooldown, ord(spell.SpellType));

        // todo: add support for other spells than projectile-based, direct/aoe/channel etc

        result.id := random(99999);
        Projectile.id := result.id; // todo: if spells die, increase this value

        Projectile.spell := spell;
        Projectile.senderstats := senderstats;
        Projectile.senderworld := senderworld;
        Projectile.x := TEntityWorld(senderworld^).x;
        Projectile.y := TEntityWorld(senderworld^).y;
        Projectile.dir := dir;
        Projectile.ttl := spell.ttl div (60 div tps) + 1;

        cs.Acquire;
        try
          len := length(prj);
          setlength(prj, len + 1);
          prj[len] := Projectile;
        finally
          cs.Release;
        end;
        result.success := true;
      end;
    end;
  end;
end;

function TSpellMan.CollisionNPC(npc: pointer): THitList;
var
  i, len: integer;
  hitdata: THitData;
begin
  setlength(result, 0);
  len := length(TNPCList(npc^)) - 1;

  cs.Acquire;
  try
    for i := 0 to len do
    begin
      if (TNPCList(npc^)[i].stats.alive) then
      begin
        hitdata := collision(TNPCList(npc^)[i].world, TNPCList(npc^)[i].stats);

        if (hitdata.collision) then
        begin
          setlength(result, length(result) + 1);
          result[length(result) - 1] := hitdata;
        end;
      end;
    end;
  finally
    cs.Release;
  end;
end;

function TSpellMan.CollisionPlayer(players: TPlayerList): THitList;
var
  i, len: integer;
  hitdata: THitData;
begin
  setlength(result, 0);
  len := length(players) - 1;

  cs.Acquire;
  try
    for i := 0 to len do
    begin
      if players[i]^.stats.alive then
      begin

        hitdata := collision(players[i]^.world, players[i]^.stats);

        if (hitdata.collision) then
        begin
          setlength(result, length(result) + 1);
          result[length(result) - 1] := hitdata;
        end;
      end;
    end;
  finally
    cs.Release;
  end;
end;

// on collide set ttl to 0; todo: use radius/size of player/object.
function TSpellMan.collision(var world: TEntityWorld; var stats: TEntityStats): THitData;
var
  j, attackdamage, spelldamage, index: integer;
  randmg: single;
begin
  result.collision := false;

  for j := 0 to high(prj) do
  begin
    // find collisions
    if (TEntityWorld(prj[j].senderworld^).entityId <> world.entityId) then
      if (sqrt(abs(Power(world.x - prj[j].x, 2) + Power(world.y - prj[j].y, 2))) < 40) then
      begin
        // kill projectile on collision
        prj[j].ttl := 0;
        result.collision := true;

        // get attack/spell damage
        attackdamage := Ceil(prj[j].spell.damage * (1 + (random(40) - 20) / 100) * TEntityStats(prj[j].senderstats^)
          .attackpower);
        spelldamage := Ceil(prj[j].spell.damage * (1 + (random(40) - 20) / 100) * TEntityStats(prj[j].senderstats^)
          .spellpower);

        // determine spelltype, damage/text-color
        case (prj[j].spell.damagetype) of
          TDamageType.Physical:
            begin
              stats.hp := stats.hp - attackdamage;
              result.value := -attackdamage;
              result.color := $FFFF0000;
            end;
          TDamageType.Magical:
            begin
              stats.hp := stats.hp - spelldamage;
              result.value := -spelldamage;
              result.color := $FFAA0078;
            end;
          TDamageType.Healing:
            begin
              stats.hp := stats.hp + spelldamage;
              result.value := spelldamage;
              result.color := $FF22CC22;
            end;
          TDamageType.Poison:
            ; // add poison affliction
        end;

        // determine fatality
        if (stats.hp < 0) then
        begin
          TEntityStats(prj[j].senderstats^).exp := TEntityStats(prj[j].senderstats^).exp + stats.level * stats.level;
          stats.alive := false;
          result.fatality := true;
        end
        else
          result.fatality := false;

        // add some stats
        result.sender_stats := prj[j].senderstats;
        result.sender_world := prj[j].senderworld;
        result.target := world.entityId;
        result.targetname := stats.name;
        result.spellname := prj[j].spell.name;
        result.projectileId := prj[j].id;
      end;
  end;
end;

// amplify speed by 1/tickrate
procedure TSpellMan.Movement;
var
  i, len: integer;
begin
  len := length(prj);

  for i := 0 to len - 1 do
  begin
    prj[i].x := prj[i].x + TSpell(prj[i].spell).speed * Cos(prj[i].dir) * (60 / tps);
    prj[i].y := prj[i].y + TSpell(prj[i].spell).speed * Sin(prj[i].dir) * (60 / tps);
    Dec(prj[i].ttl);
  end;
end;

procedure TSpellMan.Process(tps: integer);
begin
  cs.Acquire;
  try
    cooldowns.Process(tps);
    Orphan();
    Movement();
  finally
    cs.Release;
  end;
end;

constructor TSpellMan.create;
var
  bin: file of TSpellProfession;
  i, j, k: integer;
begin
  cs := TCriticalSection.create;
  setlength(prj, 0);
  cooldowns := TCooldownMan.create;

  if fileexists('data/spell.bin') then
  begin

    assignfile(bin, 'data/spell.bin');
    reset(bin);
    while not eof(bin) do
    begin
      setlength(SpellSet, length(SpellSet) + 1);
      read(bin, SpellSet[length(SpellSet) - 1]);
    end;
    closefile(bin);
  end
  else
  begin
    print('Fatal: Missing data/spell.bin');
    halt;
  end;

  for i := low(SpellSet) to High(SpellSet) do
  begin
    // print('Basic name: ' + SpellSet[i].basic.name);
    for j := low(SpellSet[i].trees) to High(SpellSet[i].trees) do
    begin
      // print('Reading Tree: ' + SpellSet[i].trees[j].name);
      for k := low(SpellSet[i].trees[j].spells) to High(SpellSet[i].trees[j].spells) do
      begin
        // print('Reading Spell: ' + SpellSet[i].trees[j].spells[k].name);
      end;
    end;
  end;

end;

end.
