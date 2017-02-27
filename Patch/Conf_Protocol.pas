unit Conf_Protocol;

interface

uses Classes, SysUtils, Utils;


const
  SEPARATOR = '&';
  SPACE = '+';
  NEWLINE = '\';

type
  TPackType = (Login = 0, Serverlist = 1, Ping = 2, Patch = 3, Disconnect = 8, Connect = 9, Character = 10, Map = 11,
    License = 12, Name = 13, account = 14, chat = 15, World = 16);

type
  TAction = (none = 0, FileData = 1, Version = 2, count = 3, filesize = 4, Success = 5, Failure = 6, Registered = 7,
    Add = 10, Download = 11, Update = 12, Movement = 13, Load = 14, Attribute = 15, Info = 16, Exist = 17,
    Unavailable = 18, Select = 20, Attributes = 21, Online = 22, Offline = 23, AlreadyLoggedOn = 24, msg = 25,
    VerifyPass = 26, Spell = 27, Collision = 28, Death = 29, Respawn = 30);

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
    sessid: string;
    PORT: integer;
    constructor create;
    destructor free;
    procedure Add(text: string);
    function parameters(): integer;
    function param(index: integer): string;
    function packetize(): string;
    function unpacketize(data: string): boolean;
    function Encoding(): {$IFDEF Win32}TEncoding{$ELSE}TIdEncoding{$ENDIF};
  end;

type
  TProtocol = class
  const
    HOST = '127.0.0.1'; // 'dungeoncube.se'; // master server.
    PORT = 1556;
  public
    sessid, user, pass, serial: string;
    aID: integer;
    logging: boolean;
    constructor create;
  end;

procedure Initialize;

var
  Protocol: TProtocol;

implementation



function TPacket.Encoding(): {$IFDEF Win32}TEncoding{$ELSE}TIdEncoding{$ENDIF};
begin
   {$IFDEF Win32}
   result := TEncoding.UTF8;
   {$ELSE}
   result := TIdEncoding.enUTF8;{$ENDIF};
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

function TPacket.packetize(): string;
var
  i: integer;
begin
  result := sessid + SEPARATOR + inttostr(ord(types)) + SEPARATOR + inttostr(ord(action));

  if (params.count > 0) then
  begin
    result := result + SEPARATOR;
    for i := 0 to params.count - 1 do
    begin
      result := result + params[i];
      if (i <> params.count - 1) then
        result := result + SEPARATOR;
    end;
    params.Clear;
  end;

  result := StringReplace(result, ' ', SPACE, [rfReplaceAll]);

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


  if Protocol.logging then
    print('<< ' + data, green);
end;

end.
