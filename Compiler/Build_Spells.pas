unit Build_Spells;

interface

{ Build script for Spells }
{$DEFINE SPELLCOMPILE}

uses Windows,
  System_Spells,
  Conf_SpriteMap,
  System_Log,
  SysUtils;

const
  MAGE_ID = 2;
  THIEF_ID = 1;
  WARRIOR_ID = 0;

procedure CompileSpells();

var
  spellprofessions: array of TSpellProfession;
  spellid: integer;

implementation

// always call defaultspell!
procedure DefaultSpell(var spell: TSpell);
begin
  spell.name := '< SPELL_NAME >';
  spell.casttime := 0;
  spell.learned := false;
  spell.damage := 0;
  spell.cooldown := 5;
  spell.ttl := 80;
  spell.speed := 6;
  spell.duration := 0;


  spell.damagetype := TDamageType.Physical;
  spell.description := '< SPELL_DESCRIPTION >';
  inc(spellid);
end;

procedure WarriorSpells(id: integer);
begin
  with spellprofessions[id] do
  begin
    profession := TProfession.Warrior;
    trees[0].name := 'Guardian';
    trees[1].name := 'Berserk';
    trees[2].name := 'Mage Hunter';

    DefaultSpell(basic);
    basic.SpellType := TSpellType.WARRIOR_BASIC;
    basic.name := 'Sword Slash';
    basic.description := 'A Basic Slash.';
    basic.cooldown := 0.8;
    basic.damage := 1.00;
    basic.ttl := 1;

    basic.targeting := TTargetType.Cone;
    basic.sprite := SPELL_WARRIOR_BASIC;
  end;
end;

procedure ThiefSpells(id: integer);
begin
  with spellprofessions[id] do
  begin
    profession := TProfession.thief;
    trees[0].name := 'Marksman';
    trees[1].name := 'Hunter';
    trees[2].name := 'Assassin';

    DefaultSpell(basic);
    basic.SpellType := TSpellType.THIEF_BASIC;
    basic.name := 'Draw Arrow';
    basic.description := 'Shoot an Arrow.';
    basic.targeting := TTargetType.Projectile;
    basic.cooldown := 0.6;
    basic.damage := 0.50;
    basic.ttl := 150;
    basic.speed := 5.5;
    basic.sprite := SPELL_THIEF_BASIC;
  end;
end;

procedure MageSpells(id: integer);
begin
  with spellprofessions[id] do
  begin
    profession := TProfession.mage;
    trees[0].name := 'Guardian';
    trees[1].name := 'Berserk';
    trees[2].name := 'Mage Hunter';

    DefaultSpell(basic);
    basic.name := 'Magic Orb';
    basic.SpellType := TSpellType.MAGE_BASIC;
    basic.description := 'Cast some Magics.';
    basic.cooldown := 1.8;
    basic.damage := 0.75;
    basic.ttl := 185;
    basic.speed := 6.8;
    basic.damagetype := TDamageType.Magical;
    basic.targeting := TTargetType.Projectile;
    basic.sprite := SPELL_MAGE_BASIC;
  end;
end;

procedure CompileSpells();
var
  i: integer;
  bin: file of TSpellProfession;
begin
  SetLength(spellprofessions, integer(High(TProfession)) + 1);
  spellid := 0;
  // numbers of professions

  print(#9 + 'Spells ...');

  WarriorSpells(0);
  ThiefSpells(1);
  MageSpells(2);

  assignfile(bin, '../data/spell.bin');
  rewrite(bin);
  for i := low(spellprofessions) to High(spellprofessions) do
    write(bin, spellprofessions[i]);
  closefile(bin);

  assignfile(bin, '../deploy/data/spell.bin');
  rewrite(bin);
  for i := low(spellprofessions) to High(spellprofessions) do
    write(bin, spellprofessions[i]);
  closefile(bin);
  print(' Complete.', lightgreen, false);
end;

end.
