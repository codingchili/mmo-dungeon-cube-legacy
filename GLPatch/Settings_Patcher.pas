unit Settings_Patcher;

interface

uses SysUtils;

const
    FILENAME = 'conf/patcher.cfg';

type
    TOption = record
        autostart, compact: boolean;
        host: string[128];
        port: integer;
    end;

type
    TPatcherSettings = class
    private
    public
        option: TOption;
        procedure load();
        procedure save();
        constructor create();
    end;

procedure Initialize;

var
    settings: TPatcherSettings;

implementation

procedure Initialize;
begin
    settings := TPatcherSettings.create;
end;

constructor TPatcherSettings.create;
begin
    option.autostart := false;
    option.compact := true;
    option.host := '127.0.0.1';
    option.port := 1576;
    load;
end;

procedure TPatcherSettings.save;
var
    sfile: file of TOption;
begin
    if not DirectoryExists('conf') then
        mkdir('conf');

    AssignFile(sfile, FILENAME);
    rewrite(sfile);
    write(sfile, option);
    closefile(sfile);
end;

procedure TPatcherSettings.load;
var
    sfile: file of TOption;
begin
    if fileexists(FILENAME) then
    begin
        AssignFile(sfile, FILENAME);
        reset(sfile);
        read(sfile, option);
        closefile(sfile);
    end;
end;

end.
