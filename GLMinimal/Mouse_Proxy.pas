unit Mouse_Proxy;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses SysUtils, Types, Forms, Classes, Controls {$IFDEF Win32}, Windows{$ENDIF Win32};

type
  TMouse = class
    class function CursorPos(): TPoint;
  end;

implementation

uses Main;

class function TMouse.CursorPos;
begin
  if (renderform.WindowState = wsNormal) then
    result := Point(Mouse.CursorPos.X - renderform.Left, Mouse.CursorPos.Y - renderform.Top)
  else
    result := Mouse.CursorPos;
  // print(inttostr(result.x) + ':' + inttostr(result.Y));
end;

end.
