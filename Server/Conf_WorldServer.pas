unit Conf_WorldServer;

interface

uses SysUtils, conf_protocol;

type
  TWorldconf = record
    masterhost, key, mode: string;
    quota, port: integer;
  end;

procedure ReadWorldConf(var wcf: TWorldconf; path: string);
function StartingTown(Profession: TProfession): string;

implementation

uses System_log;

Function StartingTown(Profession: TProfession): string;
begin
  case (Profession) of
    TProfession.Mage:
      result := 'Mage Town';
    TProfession.Warrior:
      result := 'Warrior Castle';
    TProfession.Thief:
      result := 'Thief Village';
  end;
end;

procedure ReadWorldConf(var wcf: TWorldconf; path: string);
var
  line: integer;
  confFile: TextFile;
  masterhost: string;
begin
  line := -1;
  print('Loading World Cfg..');

  try
    if not fileexists(path) then
    begin
      print(#9 + ' Failed! [Missing]', System_log.Red, false);
      print(#9 + 'File ' + path + ' is missing.' + #10 + #13 + #9 +
        'This file is vital for operation.', System_log.Red);
      readln;
      exit;
    end;

    assignFile(confFile, path);
    reset(confFile);

    inc(line);
    readln(confFile, wcf.masterhost);
    inc(line);
    readln(confFile, wcf.key);
    inc(line);
    readln(confFile, wcf.mode);
    inc(line);
    readln(confFile, wcf.quota);
    inc(line);
    readln(confFile, wcf.port);

    closefile(confFile);

    print(#9 + ' Done. [Master ' + wcf.masterhost + ':' + inttostr(wcf.port) +
      ']', System_log.LightGreen, false);
  except
    print(#9 + ' Error!', System_log.Red, false);
    print(#9 + 'File ' + path + ' is improperly configured.' + #10 + #13 + #9 +
      'An error occured at line ' + inttostr(line) + '.', System_log.Red);
    readln;
    halt;
  end;
end;

end.
