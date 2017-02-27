unit Build_CharInfo;

interface

uses Classes;

type
  TProfession = (Warrior = 0, Thief = 1, Mage = 2);

type
  TCharInfo = record
  public
    profession: TProfession;
    classname: string[16];
    description: TStringList;
  end;

type
  TCharInfoSet = Array of TCharInfo;

procedure CompileCharInfo();

implementation

uses System_log;

procedure CompileCharInfo();
var
  bin: file of TCharInfo;
  Mage, Thief, Warrior: TCharInfo;
begin
  print(#9 + 'Profession Info ...');

  Mage.profession := TProfession.Mage;
  Mage.description := TStringList.Create;
  Mage.classname := 'Mage';

  with Mage.description do
  begin
    Add('The Mage uses spells to fend of the enemy and protect their allies. The typical mage does not engage in melee combat');
    Add('and will cast their spells from a safe distance of their enemy.');
    Add('');
    Add('There are three different type of Trees/Schools which focus on different aspects of the mage, these are listed below.');
    Add('');
    Add('Protectionism/Empowerism');
    Add('As a Mage you can choose this supportive playstyle, where you have access to a number of buffs,');
    Add('that will make your allies and yourself stronger and tougher in combat.');
    Add('');
    Add('Healing,');
    Add('You may choose to play as a healer, mending damage dealt to yourself and your allies. There is no friendly fire');
    Add('so you may heal a boss your enemy is fighting, should you want. The healer combines well with protection magics from ');
    Add('the Supportive mage and/or thieves/warriors for damage.');
    Add('');
    Add('Chaos,');
    Add('The mages offensive playstyle consists of a few powerful spells that may damage enemies in an area.');
    Add('The offensive playstyle also contains some spells/abilities that can be used in close quarters.');
    Add('These abilities goes well with some of the protective spells from the Supportive school.');
    Add('');
    Add('These are just guidelines however, you may combine spells from different types of schools in any way you like.');
    Add('Learning spells of one type does not limit your spellcasting, selecting more spells of a single type will ');
    Add('later unlock a stronger ability that defines your playstyle.');
  end;

  Thief.profession := TProfession.Thief;
  Thief.description := TStringList.Create;
  Thief.classname := 'Thief';

  with Thief.description do
  begin
    Add('The Thief uses agility and shadows to evade their enemy, the common Thief can deal high amounts of damage in short periods');
    Add('and will not fight for extended periods of time. Entering and leaving the battle to stay alive, traps, poison and life drain.');
    Add('');
    Add('There are three different type of Abilities/Attacks which focus on different aspects of the thief, these are listed below.');
    Add('');
    Add('Huntsman,');
    Add('As a Thief you may play the huntsman game, you enjoy the woods and the animals within. You know where to place your traps');
    Add('for maximum yield. The huntsman blends in the nature at night, limited access to poison. May call for wolves at higher levels.');
    Add('');
    Add('Assassin,');
    Add('The assasin lurks in the shadows and moves in for the kill. The assassin is the most mobile profession. Entering and leaving');
    Add('stealth is what he does best. If you play the thief as an assasin you will apply poisons and venoms with your shivs. ');
    Add('The assasin may use backstab while in stealth, which deals bonus physical damage.');
    Add('');
    Add('Marksman,');
    Add('You are a man of the Aim, shooting many arrows for maximum damage. The marksman has less utility than the assassin and huntsman.');
    Add('The marksman makes up for lost agility with many types of arrows, shot in many different ways. If you play the thief as a ');
    Add('marksman, you do well combining abilities from assasin or huntsman for more mobility.');
    Add('');
    Add('These are just guidelines however, you may combine abilities from different types from the profession in any way you like.');
    Add('Learning abilities/attacks of one type does not limit your future abilities and attaks, selecting more spells of a single ');
    Add('type will later unlock a stronger ability that defines your playstyle.');
  end;

  Warrior.profession := TProfession.Warrior;
  Warrior.description := TStringList.Create;
  Warrior.classname := 'Warrior';

  with Warrior.description do
  begin
    Add('The Mage uses spells to fend of the enemy and protect their allies. The typical mage does not engage in melee combat');
    Add('and will cast their spells from a safe distance of their enemy.');
    Add('');
    Add('There are three different type of Trees/Schools which focus on different aspects of the mage, these are listed below.');
    Add('');
    Add('Protectionism/Empowerism');
    Add('As a Mage you can choose this supportive playstyle, where you have access to a number of buffs,');
    Add('that will make your allies and yourself stronger and tougher in combat.');
    Add('');
    Add('Healing,');
    Add('You may choose to play as a healer, mending damage dealt to yourself and your allies. There is no friendly fire');
    Add('so you may heal a boss your enemy is fighting, should you want. The healer combines well with protection magics from ');
    Add('the Supportive mage and/or thieves/warriors for damage.');
    Add('');
    Add('Chaos,');
    Add('The mages offensive playstyle consists of a few powerful spells that may damage enemies in an area.');
    Add('The offensive playstyle also contains some spells/abilities that can be used in close quarters.');
    Add('These abilities goes well with some of the protective spells from the Supportive school.');
    Add('');
    Add('These are just guidelines however, you may combine spells from different types of schools in any way you like.');
    Add('Learning spells of one type does not limit your spellcasting, selecting more spells of a single type will ');
    Add('later unlock a stronger ability that defines the type.');
  end;

  AssignFile(bin, 'charinfo.bin');
  rewrite(bin);
  write(bin, Mage);
  write(bin, Thief);
  write(bin, Warrior);
  closefile(bin);
  print(' Complete.', lightgreen, false);
end;

end.
