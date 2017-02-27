unit System_Camera;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}
// follows the player through the world.

uses Controls, Mouse_Proxy, Forms, {$IFDEF Win32}Windows,{$ENDIF} SysUtils{$IFDEF Linux}, Types{$ENDIF};

type
  Tcam = class
  private
    fx, fy: single;
    function getY(): integer;
    function getX(): integer;
    procedure setY(y: integer);
    procedure setX(x: integer);
  public
    property y: integer read getY write setY;
    property x: integer read getX write setX;
    constructor create();
    procedure Update;
    Procedure focus(x_, y_: integer);
  end;

procedure Initialize;

const
  SMOOTH: single = 0.050;
  PAN: integer = 250;

var
  cam: Tcam;

implementation

uses Engine_Player, System_Initializer, System_Log;

constructor Tcam.create();
begin
  fx := 0;
  fy := 0;
end;

function Tcam.getY(): integer;
begin
  result := round(fy);
end;

function Tcam.getX: integer;
begin
  result := round(fx);
end;

procedure Tcam.setY(y: integer);
begin
  self.fx := y;
end;

procedure Tcam.setX(x: integer);
begin
  self.fy := x;
end;

procedure Tcam.focus(x_, y_: integer);
begin
  fx := fx - initializer.swidth / 2;
  fy := fy - initializer.sheight / 2;

  fx := -abs(fx - (abs(player.x) + fx) * 1);
  fy := -abs(fy - (abs(player.y) + fy) * 1);

  fx := fx + initializer.swidth / 2;
  fy := fy + initializer.sheight / 2;
end;

procedure Tcam.Update;
var
  pos, offset: TPoint;
begin
  pos := TMouse.CursorPos;
  offset.x := 0;
  offset.y := 0;

  if (pos.x < 3) then
    offset.x := -PAN;

  if (pos.x > initializer.swidth - 3) then
    offset.x := PAN;

  if (pos.y < 3) then
    offset.y := -PAN;

  if (pos.y > initializer.sheight - 3) then
    offset.y := PAN;

  fx := fx - initializer.swidth / 2;
  fy := fy - initializer.sheight / 2;

  fx := -abs(fx - (abs(player.x + offset.x) + fx) * SMOOTH);
  fy := -abs(fy - (abs(player.y + offset.y) + fy) * SMOOTH);

  fx := fx + initializer.swidth / 2;
  fy := fy + initializer.sheight / 2;
End;

procedure Initialize;
begin
  cam := Tcam.create;
end;

END.