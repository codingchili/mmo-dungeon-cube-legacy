unit Build_Profession;

interface

uses SysUtils;

const
  WARRIOR_ID = 0;
  THIEF_ID = 1;
  MAGE_ID = 2;

type
  TProfession = record
    id: integer;
    name: string[20];
    enabled: boolean;
  end;

procedure CompileProfessions();
procedure AddProfession(name: string; enabled: boolean = true);

var
  professions: array of TProfession;

implementation

uses System_Log;

procedure AddProfession(name: string; enabled: boolean = true);
var
  len: integer;
begin
  len := length(professions);
  setlength(professions, len + 1);

  professions[len].id := len;
  professions[len].name := name;
  professions[len].enabled := enabled;
end;

procedure CompileProfessions();
var
  bin: file of TProfession;
  i: integer;
begin
  if not directoryexists('data') then
    mkdir('data');

  print(#9 + 'Professions.. ', lightgreen);

  AddProfession('Warrior');
  AddProfession('Thief');
  AddProfession('Mage');

  assignfile(bin, 'data/profession.bin');
  rewrite(bin);

  for i := low(professions) to high(professions) do
  begin
    write(bin, professions[i]);
    print(#9 + #9 + 'Writing [' + professions[i].name + ']');
  end;

  closefile(bin);
end;

end.
