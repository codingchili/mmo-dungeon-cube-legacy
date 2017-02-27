unit System_Multiplayer;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses SysUtils, Classes, {$IFDEF Win32}Windows, {$ENDIF} Conf_Protocol, System_Animation, SyncObjs, System_UtilExt, math;

// to maintain up to date records the server packet rate must be consistent.
// clear playerlist on world travel.

type
  TNetplayer = record
  public
    MovementType: TMovementType;
    sprite: TSprite;
    dir, hp, hpmax, energy, energymax, level: integer;
    alive: boolean;
    name: String;
    x, y, speed: single; // current x, y
    waypoint: TPoint; // target x, y use latest.
    aID: integer; // unique identifier, static
    profession: TProfession;
  end;

type
  TMultiplayer = class
  private
    cs: TCriticalSection;
  public
    player: array of TNetplayer;
    constructor create();
    function idpos(aID, x, y: integer): integer;
    // function getplayer(aID: integer; var player: Pointer): boolean;
    procedure spawnPlayer(aID, x, y: integer; name: string; level, hp, hpmax, mp, mpmax: integer;
      profession: TProfession);
    function getplayer(aID: integer): TNetplayer;
    procedure UpdatePlayer(aID, level, hp, hpmax, mp, mpmax: integer);
    procedure Remove(aID: integer);
    procedure Die(aID: integer);
    procedure Movepacket(Packet: TPacket);
    procedure Movement();
    procedure Animate();
    procedure Clear();
    procedure Lock();
    procedure Unlock();
  private
  end;

procedure Initialize();

var
  Multiplayer: TMultiplayer;

implementation

uses System_Log, GUI_Chat, Effects_Smoke, GUI_Targeting;

procedure TMultiplayer.Lock;
begin
  cs.Acquire;
end;

procedure TMultiplayer.Unlock;
begin
  cs.Release;
end;

constructor TMultiplayer.create();
begin
  SetLength(player, 0);
  cs := TCriticalSection.create;
end;

procedure TMultiplayer.Die(aID: integer);
var
  i: integer;
begin
  cs.Acquire;
  try
    for i := 0 to High(player) do
      if (player[i].aID = aID) then
      begin
        player[i].alive := false;
        player[i].hp := 0;
        player[i].MovementType := TMovementType.stopped;
        player[i].sprite.Animate(eAnim.aDeath, true);
      end;
  finally
    cs.Release;
  end;
end;

function TMultiplayer.getplayer(aID: integer): TNetplayer;
var
  i: integer;
begin
  result.aID := -1;
  cs.Acquire;
  for i := 0 to high(self.player) do
    if (self.player[i].aID = aID) then
    begin
      result := self.player[i];
    end;
  cs.Release;
end;

procedure TMultiplayer.Clear;
begin
  cs.Acquire;
  SetLength(player, 0);
  cs.Release;
end;

procedure TMultiplayer.Animate();
var
  i, len: integer;
begin
  cs.Acquire;
  len := high(player);
  for i := 0 to len - 1 do
    player[i].sprite.update;
  cs.Release;
end;

// remove a player from the array.
procedure TMultiplayer.Remove(aID: integer);
var
  i, j: integer;
begin
  cs.Acquire;
  try
    for i := 0 to high(player) do
      if (player[i].aID = aID) then
      begin
        SmokeMan.Add(trunc(player[i].x), trunc(player[i].y), 10, $FFFFFFFF);

        for j := i to High(player) - 1 do
          player[j] := player[j + 1];
        SetLength(player, length(player) - 1);
      end;
  finally
    cs.Release;
  end;
end;

// finds the position of the TNetplayer, if not in list added.
// at the client, random ssids may be generated if ip is known.
function TMultiplayer.idpos(aID, x, y: integer): integer;
var
  i, len: integer;
begin
  // cs.Acquire;
  try
    result := -1;
    len := length(player);

    for i := 0 to len - 1 do
    begin
      if (player[i].aID = aID) then
        result := i;
    end;

    if result = -1 then
    begin
      SetLength(player, len + 1); // add new player to module
      // print('creating player!');
      player[len].aID := aID; // listen for ssid packets

      player[len].sprite := TSprite.create('char.png', 64, 78);
      player[len].sprite.animation(5, eAnim.aStanding, 0, 0);
      player[len].sprite.animation(5, eAnim.aMovement, 0, 78);
      player[len].sprite.animation(5, eAnim.aDeath, 0, 156);
      player[len].sprite.Animate(eAnim.aStanding);
      player[len].MovementType := TMovementType.stopped;
      player[len].hp := 0;

      player[len].x := x;
      player[len].y := y;

      result := length(player) - 1; // return current posid
    end;
  finally
    // cs.Release;
  end;
end;

// todo: receive disconnect notifications, these will remove a player from the list!

procedure TMultiplayer.Movement();
var
  i, len: integer;
begin
  cs.Acquire;
  len := length(player);
  for i := 0 to len - 1 do
  begin
    if (player[i].MovementType = TMovementType.waypoint) then
    begin
      // print('waypoint is: ' + inttostr(player[i].waypoint.x) + ',' + inttostr(player[i].waypoint.y));
      player[i].dir := round(RadToDeg(arctan2(player[i].waypoint.y - player[i].y, player[i].waypoint.x - player[i].x)));
      // print('waypoint dir is: ' + inttostr(player[i].dir));

      if ((abs(player[i].x - player[i].waypoint.x) + abs(player[i].y - player[i].waypoint.y)) < player[i].speed * 2)
      then
      begin
        player[i].MovementType := TMovementType.stopped;
        player[i].sprite.Animate(eAnim.aStanding);
      end;
    end;

    if (player[i].MovementType = TMovementType.direction) or (player[i].MovementType = TMovementType.waypoint) then
    begin
      player[i].x := player[i].x + cos((6.28 / 360) * player[i].dir) * player[i].speed;
      player[i].y := player[i].y + sin((6.28 / 360) * player[i].dir) * player[i].speed;
      player[i].sprite.Animate(eAnim.aMovement);
    end;
  end;

  cs.Release;
end;

procedure TMultiplayer.UpdatePlayer(aID, level, hp, hpmax, mp, mpmax: integer);
var
  i: integer;
begin
  cs.Acquire;
  try
    i := idpos(aID, 0, 0);
    player[i].level := level;
    player[i].hp := hp;
    player[i].hpmax := hpmax;
    player[i].energy := mp;
    player[i].energymax := mpmax;
  finally
    cs.Release;
  end;
end;

// if this packet is not received, movepacket will add the player without data, in this case: ask server for more data?
// this will be updated over time by the server.
procedure TMultiplayer.spawnPlayer(aID, x, y: integer; name: string; level, hp, hpmax, mp, mpmax: integer;
  profession: TProfession); // movementtype?
var
  i: integer;
begin
  cs.Acquire;
  try
    SmokeMan.Add(x, y, 10, $FFFFFFFF);
    i := idpos(aID, x, y);
    if not(player[i].alive) then
      player[i].sprite.Animate(eAnim.aStanding);

    player[i].alive := true;
    player[i].x := x;
    player[i].y := y;
    player[i].name := name;
    player[i].level := level;
    player[i].hp := hp;
    player[i].hpmax := hpmax;
    player[i].energy := mp;
    player[i].energymax := mpmax;
    player[i].profession := profession;
  finally
    cs.Release;
  end;
end;

procedure TMultiplayer.Movepacket(Packet: TPacket);
var
  pos: integer;
  x, y, dir: integer;
  speed: single;
  MovementType: TMovementType;
begin
  cs.Acquire;
  try
    MovementType := TMovementType(ord(StrToInt(Packet.param(1))));
    Protocol.aID := StrToInt(Packet.param(2));
    dir := StrToInt(Packet.param(3));
    x := StrToInt(Packet.param(4));
    y := StrToInt(Packet.param(5));
    speed := SafeFloat(Packet.param(6));

    pos := idpos(Protocol.aID, x, y);

    if MovementType = TMovementType.waypoint then
    begin
      player[pos].waypoint.x := x;
      player[pos].waypoint.y := y;

      // calculate the dir here...
      player[pos].dir := round(RadToDeg(arctan2(player[pos].waypoint.y - player[pos].y,
        player[pos].waypoint.x - player[pos].x)));

      player[pos].speed := speed;
      player[pos].MovementType := MovementType;
    end
    else if MovementType = TMovementType.direction then
    begin
      player[pos].speed := speed;
      player[pos].MovementType := MovementType;
      player[pos].dir := dir;
    end
    else if MovementType = TMovementType.teleport then
    begin
      player[pos].speed := speed;
      player[pos].x := x;
      player[pos].y := y;
    end;
  finally
    cs.Release;
  end;
end;

procedure Initialize();
begin
  Multiplayer := TMultiplayer.create();
end;

end.
