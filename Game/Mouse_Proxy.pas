unit Mouse_Proxy;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses SysUtils, Types, Forms, Classes, Controls {$IFDEF Win32}, Windows{$ENDIF Win32};

type
  TMouse = class
    class function CursorPos(): TPoint;
  end;

implementation

uses Main, System_Log, System_Initializer;

class function TMouse.CursorPos;
begin
  if (Initializer.windowed) then
    result := renderform.ScreenToClient(Mouse.CursorPos)
  else
    result := Mouse.CursorPos;
  // print(inttostr(result.x) + ':' + inttostr(result.Y));
end;

end.
