unit Camera;

interface

//scrolls with mouse, defines cam bounds

uses Controls, Forms;

type
  Tcam = class
  X,Y, cam_speed: integer;
  x_max, x_min, y_max, y_min: integer;
 constructor create();
 procedure Update;
 Procedure focus(x_,y_: integer);
end;

var
  cam: Tcam;

implementation

uses MEMain;


constructor Tcam.create();
begin
  X := 0;
  Y := 0;
  x_min := -750;
  x_max := 60;
  y_min := -1320;
  y_max := 60;
  cam_speed := 64;
end;

procedure Tcam.focus(x_,y_: integer);
begin
  x := x_;
  y := y_;
end;

procedure Tcam.Update;
var
  mx,my: integer;
begin
  mx := 0;
  my := 0;

  if mouse.CursorPos.X < 3 then
    mx := +cam_speed;
  if mouse.CursorPos.Y < 3 then
    my := +cam_speed;
  if mouse.CursorPos.X > screen.Width-3 then
    mx := -cam_speed;
  if mouse.CursorPos.Y > screen.Height-3 then
    my := -cam_speed;

cam.X := cam.X+mx;
cam.Y := cam.Y+my;

MEMain.window.scroll_view.ScrollBy(mx, my);
End;


begin
  cam := TCam.create;
END.
