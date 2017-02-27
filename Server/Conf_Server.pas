unit Conf_Server;

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses SysUtils, Classes, System_Log, System_UtilExt;

const
  TPS = 60; // tps
  PUNCH = 16; // seconds between udp-punch
  TCP_TIMEOUT = 300;

type
  TGlobalconf = Record
  public
    hostmode: string;
  End;

procedure Initialize;

var
  globalconf: TGlobalconf;

implementation

Procedure Initialize;
var
  ConfFile: TextFile;
  values: TStringList;
  line: integer;
  tmp: string;
begin
  try
    print('Loading Global Cfg.. ');
    line := -1;

    if not fileexists('conf/global.conf') then
    begin
      print(#9 + ' Failed! [Missing]', System_Log.Red, false);
      print(#9 + 'File conf/global.conf is missing.' + #10 + #13 + #9 + 'This file is vital for operation.',
        System_Log.Red);
      readln;
      halt;
    end;

    values := TStringList.create;
    AssignFile(ConfFile, 'conf/global.conf');
    reset(ConfFile);

    while not(eof(ConfFile)) do
    begin
      inc(line);
      readln(ConfFile, tmp);
    end;
    Split('=', tmp, values);

    if values[0] = 'hosting' then
      globalconf.hostmode := values[1];

    values.Free;

    print(#9 + ' Done.', System_Log.LightGreen, false);
  except
    print(#9 + ' Error!', System_Log.Red, false);
    print(#9 + 'File conf/global.conf is improperly configured.' + #10 + #13 + #9 + 'An error occured at line ' +
      inttostr(line) + '.', System_Log.Red);
    readln;
    halt;
  end;
end;

end.
