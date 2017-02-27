unit System_Spells;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses SysUtils, Math{$IFNDEF SPELLCOMPILE} , Controls, Mouse_Proxy, dialogs, Conf_Protocol{$ENDIF}, Types;

type
  TTargetType = (Projectile = 0, Area = 1, Explosion = 2, Self = 3, Cone = 4); // used for line of missile

type
  TSpellType = (WARRIOR_BASIC = 0, THIEF_BASIC = 1, MAGE_BASIC = 2, EMPTY = -1);

type
  TSpellState = (Available, Unavailable, Energy, Cooldown);

type
  TDamageType = (Physical, Magical, Poison, Full, Healing);

{$IFDEF SPELLCOMPILE}

type
  TProfession = (Thief, Warrior, Mage);
{$ENDIF}

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
    trees: array [0 .. 2] of TSpellTree;
    profession: TProfession;
    basic: TSpell; // basic attacks are not a part of a tree
  end;

type
  TSpellMan = class
  public
    SpellCount: integer;
    SpellSet: array of TSpellProfession;
{$IFNDEF SPELLCOMPILE}
    procedure Process();
    constructor create;
    function Cast(var spell: TSpell): boolean;
    procedure Aim(var spell: TSpell);
    function SpellState(spell: TSpell): TSpellState;
    function getSpell(id: integer; var spell: TSpell): boolean;
{$ENDIF}
  end;

{$IFNDEF SPELLCOMPILE}

procedure Initialize;
{$ENDIF}

var
  SpellMan: TSpellMan;

implementation

{$IFNDEF SPELLCOMPILE}

uses System_Log, Conf_Spritemap, GUI_SkillBar, Main, Network_World, System_Camera,
  Engine_player{$IFDEF Win32}, System_Audio{$ENDIF};

procedure Initialize;
begin
  SpellMan := TSpellMan.create;
end;

function TSpellMan.getSpell(id: integer; var spell: TSpell): boolean;
var
  i, j: integer;
  profession: TProfession;
begin
  result := false;

  for profession := low(TProfession) to high(TProfession) do
    if (integer(SpellSet[ord(profession)].basic.SpellType) = id) then
    begin
      spell := SpellSet[ord(profession)].basic;
      result := true;
    end
    else
      for i := 0 to high(SpellSet[ord(profession)].trees) do
        for j := 0 to high(SpellSet[ord(profession)].trees[i].spells) do
          if (ord(SpellSet[ord(profession)].trees[i].spells[j].SpellType) = id) then
          begin
            result := true;
            spell := SpellSet[ord(profession)].trees[i].spells[j];
          end;
end;

constructor TSpellMan.create;
var
  bin: file of TSpellProfession;
  i, j, k: integer;
begin
  if fileexists('data/spell.bin') then
  begin

    assignfile(bin, 'data/spell.bin');
    reset(bin);
    while not eof(bin) do
    begin
      SetLength(SpellSet, length(SpellSet) + 1);
      read(bin, SpellSet[length(SpellSet) - 1]);
    end;
    CloseFile(bin);
  end
  else
  begin
    showmessage('Fatal: Missing data/spell.bin');
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

function TSpellMan.Cast(var spell: TSpell): boolean;
var
  dir: single;
begin
  result := false;

  if (SpellState(spell) = TSpellState.Available) or (true) and not(spell.SpellType = TSpellType.EMPTY) then
  begin
    result := true;
    spell.timer := round(spell.Cooldown * Main.speed);
    dir := arctan2(TMouse.cursorpos.Y - cam.Y - player.Y, TMouse.cursorpos.X - cam.X - player.X);
    WorldConnection.CastSpell((ord(spell.SpellType)), dir);
  end;

  // draw locally or send to server?
  // hmm.. draw locally saves bandwidth
  // bounce provides more rubberbanding.
  // ProjectileMan.fire();
end;

// set target spell, on mouse click Left fire! (cast(loaded_id))
procedure TSpellMan.Aim(var spell: TSpell);
begin
  // show line of missile
  print('Aiming Spell #' + inttostr(ord(spell.SpellType)));
end;

// set spell availability here.
procedure TSpellMan.Process;
var
  i: integer;
begin
  // process cooldowns unless silenced
  for i := low(SkillBar.skills) to High(SkillBar.skills) do
  begin
    if (SkillBar.skills[i].spell^).timer > 0 then
      dec(SkillBar.skills[i].spell^.timer);
  end;
end;

// check if hasmana and if learned
function TSpellMan.SpellState(spell: TSpell): TSpellState;
begin
  result := TSpellState.Unavailable;

  if (spell.timer < 1) then
    result := TSpellState.Available;
end;

{$ENDIF}

end.
