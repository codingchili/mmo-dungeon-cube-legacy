unit Conf_Names;

interface

uses SysUtils, Classes, System_UtilExt;

type
  TReservedName = record
    name: string;
    serial: string[25];
  end;

type
  TReservedNames = class
  private
    reservations: array of TReservedName;
  public
    constructor create(const filename: string);
    procedure sanitize(var name: string);
    function reserved(const name: string): boolean;
  end;

procedure Initialize;

var
  reservednames: TReservedNames;

implementation

procedure Initialize;
begin
  reservednames := TReservedNames.create('conf/reserved_names.conf');
end;

constructor TReservedNames.create(const filename: string);
var
  conf: TextFile;
  line: string;
  param: TStrings;
  len: integer;
begin
  //param := TStrings.create;

  param := TStringList.create;

  if fileexists(filename) then
  begin
    assignFile(conf, filename);
    reset(conf);

    while not(eof(conf)) do
    begin
      readln(conf, line);

      inc(len);
      setlength(reservations, len + 1);
      Split('=', line, param);
      reservations[len].serial := param[0];
      reservations[len].name := param[1];
      param.Clear;
    end;
  end;
end;

procedure TReservedNames.sanitize(var name: string);
begin

end;

function TReservedNames.reserved(const name: string): boolean;
begin

end;

end.
