unit System_ConsoleEffect;

interface

uses Windows, System_Log;

procedure Clear;

implementation

procedure Clear;
var
  hStdOut: HWND;
  ScreenBufInfo: TConsoleScreenBufferInfo;
  Coord1: TCoord;
  z: Integer;
begin
  hStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(hStdOut, ScreenBufInfo);

  for z := 1 to ScreenBufInfo.dwSize.Y do
    print('');

  Coord1.X := 0;
  Coord1.Y := 0;
  SetConsoleCursorPosition(hStdOut, Coord1);
end;

end.
