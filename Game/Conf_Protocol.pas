unit Conf_Protocol;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses Classes, SysUtils, IdGlobal;

const
  SEPARATOR: char = '&';
  SPACE: char = '+';
  NEWLINE: char = '\';
  MAXBYTES = 512;

type
  TPackType = (Login = 0, Serverlist = 1, Ping = 2, Patch = 3, Disconnect = 8, Connect = 9, Character = 10, Map = 11,
    License = 12, Name = 13, account = 14, chat = 15, World = 16);

type
  TAction = (none = 0, FileData = 1, Version = 2, count = 3, filesize = 4, Success = 5, Failure = 6, Registered = 7,
    Add = 10, Download = 11, Update = 12, Movement = 13, Load = 14, Attribute = 15, Info = 16, Exist = 17,
    Unavailable = 18, Select = 20, Attributes = 21, Online = 22, Offline = 23, AlreadyLoggedOn = 24, msg = 25,
    VerifyPass = 26, Spell = 27, Collision = 28, Death = 29, Respawn = 30, Spawn = 31, Token = 32);

type
  TMovementType = (waypoint = 0, teleport = 1, direction = 2, stopped = 3);

type
  TPlayerType = (MultiplayerType = 0, PlayerType = 1);

  // can be obtained through achievments or items? add more later..
type
  TTextEffect = (normal = 0, fade = 1, explode = 2);

type
  TMessageType = (sys = 0, pm = 1, text = 2);

type
  TProfession = (Warrior = 0, Thief = 1, Mage = 2);

type
  TAttribute = (Health = 0, Energy = 1, Experience = 2);

type
  TProjectileType = (TRAP = 0, SHIELD = 1, STEALTH = 2, ICEBALL = 3, BTHIEF = 4, BWARRIOR = 5, BMAGE = 6, FIREBALL = 7);

type
  TDirection = (Up = 0, Down = 1, Left = 2, Right = 3, Null = 4, upleft = 5, upright = 6, downleft = 7, downright = 8);

type
  TPacket = class
  private
    params: TStringList;
  public
    types: TPackType;
    action: TAction;
    ip: string[18];
    sessid: string[25];
    PORT: integer;
    constructor create;
    destructor free;
    procedure Add(text: string);
    function parameters(): integer;
    function param(index: integer): string;
    function packetize(): TIdBytes;
    function unpacketize(data: string): boolean;
    class function Encoding(): IdTextEncodingType;
  end;

type
  TPacketBundle = array of TPacket;

type
  TProtocol = class
  const
    HOST = '127.0.0.1'; // 'dungeoncube.se'; // master server.
    PORT = 1556;
  public
    sessid, user, pass, serial: string;
    aID: integer;
    logging: boolean;
    inbytes, outbytes, intotal, outtotal: integer;
    ipolls, opolls: integer; // every 60 polls reset
    constructor create;
    function netIOO(): string;
    function netIOI(): string;
    function getPackets(payload: string): TPacketBundle;
  end;

procedure Initialize;

var
  Protocol: TProtocol;

implementation

uses System_Log, System_UtilExt, Debug_Stopwatch;

function TProtocol.getPackets(payload: string): TPacketBundle;
var
  data: TStringList;
  packets: TPacketBundle;
  i: integer;
begin
  data := TStringList.create;
  data.text := payload;

  for i := 0 to data.count - 1 do
  begin
    setlength(packets, i + 1);
    packets[i] := TPacket.create;
    packets[i].unpacketize(data[i]);
  end;

  data.free;
  result := packets;
end;

function TProtocol.netIOO;
begin
  if logging then
  begin
    inc(opolls);
    if (opolls > 60) then
    begin
      opolls := 0;
      cs.Acquire;
      outtotal := outbytes;
      outbytes := 0;
      cs.Release;
    end;
    result := FormatFloat('0.0#', outtotal / 1000) + ' KB/s';
  end
  else
    result := '';
end;

function TProtocol.netIOI;
begin
  if logging then
  begin
    inc(ipolls);
    if (ipolls > 60) then
    begin
      ipolls := 0;
      cs.Acquire;
      intotal := inbytes;
      inbytes := 0;
      cs.Release;
    end;
    result := FormatFloat('0.0#', intotal / 1000) + ' KB/s';
  end
  else
    result := '';
end;

class function TPacket.Encoding(): IdTextEncodingType;
begin
  result := IdTextEncodingType.encUTF8;
end;

procedure Initialize;
begin
  Protocol := TProtocol.create;
end;

constructor TProtocol.create;
begin
  sessid := '';
  logging := false;
end;

constructor TPacket.create();
begin
  params := TStringList.create;
  sessid := Protocol.sessid;
end;

destructor TPacket.free;
begin
  params.free;

  if self <> nil then
    inherited destroy;
end;

function TPacket.parameters: integer;
begin
  result := self.params.count;
end;

procedure TPacket.Add(text: string);
begin
  if (length(text) > 0) then

    params.Add(text)
  else
    params.Add('0');
end;

function TPacket.param(index: integer): string;
begin
  if (params.count > index) then
    result := params[index]
  else
    result := '0';
end;

function TPacket.packetize(): TIdBytes;
var
  i: integer;
  astext: string;
  data: TIdBytes;
  len: word;
begin
  astext := sessid + SEPARATOR + IntToStr(ord(types)) + SEPARATOR + IntToStr(ord(action));

  if (params.count > 0) then
  begin
    astext := astext + SEPARATOR;
    for i := 0 to params.count - 1 do
    begin
      astext := astext + params[i];
      if (i <> params.count - 1) then
        astext := astext + SEPARATOR;
    end;
    params.Clear;
  end;

  astext := StringReplace(astext, ' ', SPACE, [rfReplaceAll]);

  if Protocol.logging then
  begin
    print('>> ' + astext, yellow);
    Protocol.outbytes := Protocol.outbytes + length(astext);
  end;

  data := IndyTextEncoding(Encoding).GetBytes(astext);
  len := length(data);
  setlength(result, len + 2);
  move(len, result[0], 2);
  move(data[0], result[2], len);
end;

function TPacket.unpacketize(data: string): boolean;
begin
  result := true; // check results with sessID

  Split(SEPARATOR, data, params);

  if params.count > 2 then
  begin
    sessid := params[0];
    types := TPackType(StrToInt(params[1]));
    action := TAction(StrToInt(params[2]));

    params.text := StringReplace(params.text, SPACE, ' ', [rfReplaceAll]);

    if params.count > 2 then
    begin
      params.Delete(0);
      params.Delete(0);
      params.Delete(0);
    end;
  end;


  // if sessid = '' then sessid=packet.sessid
  // if packet.sessid = sessid result = true

  if Protocol.logging then
  begin
    print('<< ' + data, green);
    Protocol.inbytes := Protocol.inbytes + length(data);
  end;
end;

end.