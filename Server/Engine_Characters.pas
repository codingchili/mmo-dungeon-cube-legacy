unit Engine_Characters;

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses Conf_Protocol, SysUtils, System_Log, IdContext, SyncObjs {, Windows};

type
  TCharacterList = array [0 .. Integer(High(TProfession))] of Pointer;

type
  TPoint = Record
    X: Integer;
    Y: Integer;
  end;

type
  TEntityWorld = Record
    X, Y: single;
    entityId: Integer;
    movementtype: TMovementType;
    waypoint: TPoint;
    dir: Integer;
  End;

type
  TEntityState = record
    update, mpupdate: boolean;
    lasthpupdate, lastmpupdate, lasthpbroadcast, lastmpbroadcast, lastexpupdate: single;
    // hpupdatedelta, hpbroadcastdelta, mpbroadcastdelta, mpupdatedelta: single;
    lastpunch: Integer;
  end;

type
  TEntityNetwork = Record
    Sock: TIdContext;
    ip: string[16]; // beware! no ipv6!
    update, mpupdate: boolean;
    port: Integer;
  End;

type
  TEntityStats = Record
    name: String[24];
    profession: TProfession;
    alive: boolean;
    level, hpmax, energymax, attackpower, spellpower, pr, mr, exp, nexp: Integer;
    hp, energy, speed: single;
    INT, STR, WIS, CON, DEX, SPR: Integer;
    attrpts, skillpts: Integer;
  End;

type
  TCharacter = Record
    sessid: string[SESSID_LENGTH];
    user: string[USERNAME_MAXLEN];
    pass: string[PASSWORD_MAXLEN];
    map: String[16];
    equipped: array [0 .. 9] of Integer;
    inventory: array [0 .. 42] of Integer;
    world: TEntityWorld;
    network: TEntityNetwork;
    stats: TEntityStats;
    state: TEntityState;
  End;

type
  TCharacterDB = class
  private
    nextid, charlen: Integer;
    char: array [0 .. 15000] of TCharacter;
    temporary: boolean;
    cs: TCriticalSection;
  public
    // load and save character database.
    procedure Load();
    procedure Save();

    // create a new character
    function new(aID: Integer; name: string; profession: TProfession): Pointer;

    // return character with aID and profession
    function Character(aID: Integer; profession: TProfession; var Character: Pointer): boolean;

    // return number of registered accounts
    function registered(): Integer;

    // list all characters an account has
    procedure CharacterList(aID: Integer; var CharacterList: TCharacterList);

    // on level up
    procedure LevelUp(var stats: TEntityStats; var state: TEntityState);
    procedure CalculateStats(var stats: TEntityStats; var state: TEntityState);

    // constructor
    constructor Create(temp: boolean);
  end;

type
  TPlayerList = Array of ^TCharacter;

var
  CharDB: TCharacterDB;

implementation

uses Conf_WorldServer, math, Engine_Tick;

function TCharacterDB.registered;
begin
  cs.Acquire;
  try
    result := charlen;
  finally
    cs.release;
  end;
end;

procedure TCharacterDB.LevelUp(var stats: TEntityStats; var state: TEntityState);
var
  overlevel: Integer;
begin
  with stats do
  begin
    overlevel := exp;

    repeat
      level := level + 1;
      exp := 0;
      nexp := (level * level * level) * 10;

      attrpts := round((level / 5) + level * 5);
      skillpts := level * 5;

      // lolwut
      STR := STR + 20;

      CalculateStats(stats, state);
      overlevel := overlevel - nexp;
    until overlevel < nexp;

    exp := abs(overlevel);
  end;
end;

procedure TCharacterDB.CalculateStats(var stats: TEntityStats; var state: TEntityState);
begin
  with stats do
  begin
    hpmax := CON * 10 + level * level;
    energymax := 50 + SPR * 2;
    speed := 1.3;
    attackpower := STR * 2 + DEX * 1;
    spellpower := INT * 4;
    mr := CON * 1 + SPR * 1;
    pr := CON * 1 + DEX * 1;

    // todo calculate equipped stats.
  end;

  state.update := true;
  state.mpupdate := true;
end;

procedure TCharacterDB.CharacterList(aID: Integer; var CharacterList: TCharacterList);
var
  i: Integer;
begin
  for i := 0 to high(CharacterList) do
    CharacterList[i] := NIL;

  cs.Acquire;
  try
    for i := 0 to charlen - 1 do
    begin
      if (char[i].world.entityId = aID) then
      begin
        CharacterList[Ord(char[i].stats.profession)] := @char[i];
      end;
    end;
  finally
    cs.release;
  end;
end;

function TCharacterDB.Character(aID: Integer; profession: TProfession; var Character: Pointer): boolean;
var
  i: Integer;
begin
  Character := NIL;
  cs.Acquire;
  try
    result := false;

    for i := 0 to charlen - 1 do
    begin
      if (char[i].world.entityId = aID) and (char[i].stats.profession = profession) then
      begin
        Character := @char[i];
        result := true;
      end;
    end;
  finally
    cs.release;
  end;
end;

function TCharacterDB.new(aID: Integer; name: string; profession: TProfession): Pointer;
var
  Character: ^TCharacter;
  index: Integer;
begin
  cs.Acquire;
  try
    Character := @char[charlen];
    inc(charlen);
    index := charlen;
  finally
    cs.release;
  end;

  with Character^ do
  begin
    stats.alive := false;
    map := StartingTown(profession);

    // set up level
    stats.exp := 0;
    stats.nexp := 5;
    stats.level := 1;

    // name and profession
    stats.name := name;
    stats.profession := profession;

    // world entity
    world.entityId := aID;
    world.X := 0;
    world.Y := 0;

    // base attributes
    stats.CON := 5;
    stats.INT := 5;
    stats.WIS := 5;
    stats.DEX := 5;
    stats.SPR := 5;
    stats.STR := 5;

    world.movementtype := TMovementType.stopped;
    CalculateStats(stats, state);
  end;

  result := @char[index];
end;

constructor TCharacterDB.Create(temp: boolean);
var
  db: File of TCharacter;
begin
  cs := TCriticalSection.Create;
  charlen := 0;

  if fileexists('data/character.data') and (temp = false) then
  begin
    temporary := false;
    assignFile(db, 'data/character.data');
    reset(db);

    while not(eof(db)) do
    begin
      read(db, char[charlen]);
      inc(charlen);
    end;
    closefile(db);
  end;
end;

procedure TCharacterDB.Load();
var
  savefile: file of TCharacter;
begin
  print('Reading Char Data..', Brown);
  charlen := 0;

  if not(fileexists('data/character.data')) then
  begin
    print(#9 + ' No Data.', LightRed, false);
    exit;
  end;

  assignFile(savefile, 'data/' + 'character.data');
  reset(savefile);

  while not eof(savefile) do
  begin
    read(savefile, char[charlen]);
    inc(charlen);
  end;

  closefile(savefile);
  print(#9 + ' Done.', LightGreen, false);
end;

procedure TCharacterDB.Save();
var
  db: file of TCharacter;
  i: Integer;
begin
  print('Writing data..');
  if not temporary then
  begin

    assignFile(db, 'data/character.data');
    rewrite(db);

    for i := 0 to charlen - 1 do
    begin
      write(db, char[i]);
    end;
    closefile(db);
    print(#9 + ' Done.', System_Log.LightGreen, false);
    print('');
  end;
end;

end.
