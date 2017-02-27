unit Engine_Player;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses Rectarea, Graphics, {$IFDEF Win32}Windows, {$ENDIF} SysUtils, Controls, Math, Conf_Protocol, IdUDPServer,
  IdGlobal,
  IdSocketHandle, System_Animation, Classes;

type
  TStat = Record
  public
    INT, DEX, STR, SPR, ACC, CON: integer;
  end;

type
  TPlayer = class
  private
    lastdir: single;
  public
    alive: boolean;
    x, y, speed, health, maxhealth, energy, maxenergy, experience, maxexperience: single;
    id, level, attackpower, spellpower, skillpts, statpts, prdef, mrdef: integer;
    stat: TStat;
    size, aID: integer;
    profession: TProfession;
    name: string;
    sprite: TSprite;
    constructor create;
    procedure movement();
    function Bounds(player: TPlayer; cam_x, cam_y: integer): Rectarea.TRect;
    procedure Setexp(amount: integer);
    procedure die();
    function getX(): integer;
    function getY(): integer;
    function getspeed(): single;
    class function getBounds(x, y, cam_x, cam_y, size: integer): Rectarea.TRect;
  end;

var
  player: TPlayer;

implementation

Uses System_Initializer, Network_World, System_log, System_Keyboard,
  GUI_Inventory, Effects_Splat,
  GUI_StatusBox;

class function TPlayer.getBounds(x, y, cam_x, cam_y, size: integer): Rectarea.TRect;
begin
  result := Rectarea.new(trunc(x + cam_x - size / 2), trunc(y + cam_y - size / 2), size, player.size);
end;

function TPlayer.Bounds(player: TPlayer; cam_x, cam_y: integer): Rectarea.TRect;
begin
  result := Rectarea.new(trunc(player.x + cam_x - player.size / 2), trunc(player.y + cam_y - player.size / 2),
    trunc(player.size), trunc(player.size));
End;

constructor TPlayer.create;
begin
  id := 0;
  x := 700;
  y := 700;
  health := 0;
  maxhealth := 300;
  speed := 2.4;
  level := 1;
  name := 'Error: Please Relog.';
  size := 70;

  statpts := 0;
  skillpts := 0;
  alive := true;

  stat.STR := 5; // lv10 = 90 attack power, 350hp,
  stat.INT := 5; // lv10 = 40 attack power, 800hp
  stat.CON := 5;
  stat.ACC := 5;
  stat.DEX := 5;
  stat.SPR := 5; // Character^.CON * 10 + Character^.level * 25 + 50;

  sprite := TSprite.create('char.png', 64, 78);
  sprite.animation(5, eAnim.aStanding, 0, 0);
  sprite.animation(5, eAnim.aMovement, 0, 78);
  sprite.animation(5, eAnim.aDeath, 0, 156);
  sprite.animate(eAnim.aStanding);
End;

procedure TPlayer.Setexp(amount: integer);
begin
  experience := experience + amount;
End;

function TPlayer.getX: integer;
begin
  result := trunc(x);
end;

function TPlayer.getY: integer;
begin
  result := trunc(y);
end;

function TPlayer.getspeed(): single;
begin
  result := speed;
end;

procedure TPlayer.die();
begin
  player.sprite.animate(eAnim.aDeath, true);
  SplatMan.Splat(TSplatEffect.BloodSplat);
  player.alive := false;
  player.health := 0;

  StatusBox.settext('You have left the realm of the living.');
  StatusBox.show;
  StatusBox.finished(WorldConnection.respawn); // set action logout
  // show respawn dialog
end;

// (6.28 / 360) * DEGREES = dir
procedure TPlayer.movement();
var
  dir: integer;
  w, a, s, d: boolean;
begin
  // player.experience := player.experience + 15;
  // CharStats.refresh;

  if (player.alive) then
  begin
    dir := -1;
    w := (Keyboard.chars[ord('W')]); // not(GetAsyncKeyState(ord('W')) = 0);
    a := (Keyboard.chars[ord('A')]); // not(GetAsyncKeyState(ord('A')) = 0);
    s := (Keyboard.chars[ord('S')]); // not(GetAsyncKeyState(ord('S')) = 0);
    d := (Keyboard.chars[ord('D')]); // not(GetAsyncKeyState(ord('D')) = 0);

    if (w = true) then
      dir := 270;
    if (a = true) then
      dir := 180;
    if (s = true) then
      dir := 90;
    if (d = true) then
      dir := 0;

    if (w = true) and (a = true) then
      dir := 225;
    if (w = true) and (d = true) then
      dir := 315;

    if (s = true) and (a = true) then
      dir := 135;
    if (s = true) and (d = true) then
      dir := 45;

    if (dir <> -1) then
    begin
      x := x + cos((6.28 / 360) * dir) * player.speed;
      y := y + sin((6.28 / 360) * dir) * player.speed;
    end;

    if (lastdir <> -1) and (dir = -1) then
    begin
      WorldConnection.UpdateMovement(-1, getX, getY, TMovementType.waypoint);
      player.sprite.animate(eAnim.aStanding);
    end;

    if (dir <> -1) and (lastdir <> dir) then
    begin
      WorldConnection.UpdateMovement(trunc(dir), getX, getY, TMovementType.direction);
      player.sprite.animate(eAnim.aMovement);
    end;

    // update over network... if dir = -1 and lastdir <> -1 then send waypoint (stopped moving)
    lastdir := dir;
  end
  else
    SplatMan.Splat(TSplatEffect.BloodSplat);
End;

END.
