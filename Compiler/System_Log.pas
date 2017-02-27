unit System_Log;

interface

uses Windows, Classes, SyncObjs;

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

procedure print(text: string; color: integer = 6; newline: boolean = TRUE);
procedure Initialize;

var
  logging: boolean = TRUE;
  cs: TCriticalSection;

implementation

procedure Initialize;
begin
  cs := TCriticalSection.Create;
end;

procedure toggle;
begin
  if logging = TRUE then
  begin
    logging := False;
    print('Logging ', Brown, False);
    print(#9 + ' OFF.', LightRed)
  end
  else
  begin
    logging := TRUE;
    print('Logging ', Brown, False);
    print(#9 + ' ON.', LightGreen)
  end;
end;

procedure print(text: string; color: integer = 6; newline: boolean = TRUE);
begin
  if logging = TRUE then
  begin
    try
      cs.Acquire;

      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);

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
