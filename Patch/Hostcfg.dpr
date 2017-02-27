library Hostcfg;

uses
  SimpleShareMem,
  System.SysUtils;

{$R *.res}

var
hostfile: textFile;

procedure SetHost(Host:String); export;
begin
AssignFile(hostfile, 'conf/Host.cfg');
rewrite(hostfile);
writeln(hostfile, host);
writeln(hostfile, '80');
closefile(hostfile);
end;

exports SetHost;

begin
end.
