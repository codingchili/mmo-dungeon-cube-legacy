unit Network_Login;

interface

// TODO: add a thread to scan for void connections, delete them and alert all
// clients. update their clientlist too, by rmUsr cmd.

uses IdContext, Engine_Accounts, System_Log, SysUtils, Network_MetaServer,
  System_Vars,
  Classes, SyncObjs;

procedure initialize;
procedure Login(packet: TPacket; AContext: TIdContext; user, pass: string);
procedure Registration(packet: TPacket; AContext: TIdContext);

var
  AccountDB: TAccountDB;

implementation

procedure initialize;
begin
  AccountDB := TAccountDB.create;
  AccountDB.load;
end;

procedure Registration(packet: TPacket; AContext: TIdContext);
var
  reply: TPacket;
  registered: boolean;
  i: integer;
begin
  reply.param := TStringList.create;
  reply.types := TPackType.Login;

  if (length(packet.param[0]) < USERNAME_LENGTH) or
    (length(packet.param[0]) > USERNAME_MAXLEN) then
  begin
    reply.param.Add('Username length mismatch ' + inttostr(USERNAME_LENGTH) +
      '-' + inttostr(USERNAME_MAXLEN) + ' characters expected.');
    reply.action := TAction.Msg;
  end
  else if (length(packet.param[1]) < PASSWORD_LENGTH) or
    (length(packet.param[0]) > PASSWORD_MAXLEN) then
  begin
    reply.param.Add('Password length mismatch ' + inttostr(PASSWORD_LENGTH) +
      '-' + inttostr(PASSWORD_MAXLEN) + ' characters expected.');
    reply.action := TAction.Msg;
  end
  else if (length(packet.param[2]) < SERIAL_LENGTH) or
    (length(packet.param[2]) > SERIAL_MAXLEN) then
  begin
    reply.param.Add
      ('Serial length mismatch, group by 5 separated with hyphen or not.' +
      inttostr(SERIAL_LENGTH) + '-' + inttostr(SERIAL_MAXLEN) +
      ' characters expected.');
    reply.action := TAction.Msg;
  end
  else
  begin

    // check if registration key is valid or taken.
    // AccountDB.serial
    // System_Licensing.valid
    registered := true; // registered = AccountDB user pass

    // theres no key with id, or theres no id with user, then create the user.
    if registered = false then
    begin
      print('Registered Character <' + packet.param[0] + '> With Pass <' +
        packet.param[1] + '>.', System_Log.green);
      // accountDB.register;
      reply.action := TAction.registered;

    end;
  end;

  AContext.Connection.Socket.WriteLn(packetize(reply), TEncoding.Unicode);
  reply.param.Free;
end;

// let the user create accounts.
// replace this function with the TAccountDB.Connect.
procedure Login(packet: TPacket; AContext: TIdContext; user, pass: string);
var
  i, pos: integer;
  auth: boolean;
  reply: TPacket;
begin
  auth := false;
  reply.param := TStringList.create;

  // todo: implement binary search.
  for i := 0 to high(AccountDB.offline) do
  begin
    if (AccountDB.offline[i].user = user) and (AccountDB.offline[i].pass = pass)
    then
    begin
      print('User Logon <' + user + '>.', System_Log.green);
      AccountDB.connect(i);
      auth := true;
      if AccountDB.loggedon(i) then
      begin
        reply.types := TPackType.Login;
        reply.action := TAction.AlreadyLoggedOn;
      end
      else
      begin
        reply.types := TPackType.Login;
        reply.action := TAction.Success;
      end;
      AContext.Connection.Socket.WriteLn(packetize(reply), TEncoding.Unicode);
      break;
    end;
  end;

  if not(auth) then
  begin
    packet.types := TPackType.Login;
    packet.action := TAction.Failure;
    AContext.Connection.Socket.WriteLn(packetize(packet), TEncoding.Unicode);
    print('Failed Login <' + user + '> With Pass <' + pass + '>.',
      System_Log.Red);
  end;
end;

end.
