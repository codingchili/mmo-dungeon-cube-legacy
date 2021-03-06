unit System_Log;

interface

uses {$IFDEF Win32}Windows,{$ENDIF} {$IFDEF Linux}crt, {$ENDIF} Classes, SyncObjs, SysUtils;

const
  Black = 0;
  Blue = 1;
  Green = 2;
  Cyan = 3;
  Red = 4;
  Magenta = 5;
  Brown = 6;
  LightGray = 7;
  DarkGray = 8;
  LightBlue = 9;
  LightGreen = 10;
  LightCyan = 11;
  LightRed = 12;
  LightMagenta = 13;
  Yellow = 14;
  White = 15;

procedure print(const text: string; color: integer = 6; newline: boolean = true);
procedure Initialize;

var
  logging: boolean = True;
  cs: TCriticalSection;

implementation

procedure Initialize;
begin
  cs := TCriticalSection.create;
end;

procedure toggle;
begin
  if logging = true then
  begin
    logging := False;
    print('Logging ', Brown, False);
    print(#9 + ' OFF.', LightRed)
  end
  else
  begin
    logging := true;
    print('Logging ', Brown, False);
    print(#9 + ' ON.', LightGreen)
  end;
end;

procedure print(const text: string; color: integer = 6; newline: boolean = true);
begin
  if logging then
  begin
    cs.Acquire;
    try

      {$IFDEF Win32}
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
      {$ENDIF}
      {$IFDEF Linux}
      TextColor(color);
      {$ENDIF}


      if newline then
      begin
        write(#13 + #10 + text);
      end
      else
        write(text);
    finally
      cs.Release;
    end;
  end;
end;

end.