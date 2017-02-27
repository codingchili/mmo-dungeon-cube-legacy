unit Engine_Accounts;

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

interface

uses {Windows,} System_Licensing, System_Log,
    Sysutils, SyncObjs, Conf_Protocol, IdContext, Classes, Engine_Characters;

type
    TAccount = record
        id: integer;
        online: boolean;
        serial: string[30];
        user: string[USERNAME_MAXLEN];
        pass: string[PASSWORD_MAXLEN];
        ip: string[16];
        date: string[18];
    end;

    // context data pointer
type
    TSession = class
    public
        authenticated: boolean;
        account: ^TAccount;
        character: ^TCharacter;
    end;

type
    TAccountDB = class
    public
        nextid, acclen: integer;
        cs: TCriticalSection;
        account: array [0 .. 5000] of TAccount; // linked list of accounts
        // index of accounts, dynamic array, sorted by username?
        // please hash the passwords
        constructor create();
        function connect(var account: TAccount; const ip: string; const port: word): boolean;
        procedure disconnect(var account: TAccount);
        procedure load();
        procedure save();
        function add(user, pass, serial, ip: string; id: integer = -1): TAccount;
        procedure Registration(var packet: TPacket; var Context: TIdContext);
        procedure SendAccounts(var Context: TIdContext);
        // procedure ReadAccounts(var packet: TPacket; const ip: string; const port: word);
        procedure Login(var packet: TPacket; var Context: TIdContext; const user, pass: string);
        function registered(const serial: string): boolean;
        function loggedon(id: integer): boolean;
        function position(var stack: array of TAccount; id: integer): integer;
        function license(const serial: string): boolean;
        function username(const name: string): boolean;
        function accountnum(): integer;
        function VerifyInput(var packet, reply: TPacket; license: boolean = true): boolean;
        function Authenticate(const user: string; const pass: string; var account: Pointer;
          sender: String = 'Meta'): boolean;
    end;

procedure initialize;

var
    AccountDB: TAccountDB;

implementation

uses System_UtilExt, Conf_ServerList, NetWork_BackEnd, System_Token;

function TAccountDB.accountnum;
begin
    cs.Acquire;
    result := acclen;
    cs.Release;
end;

procedure TAccountDB.SendAccounts(var Context: TIdContext);
var
    packet: TPacket;
    i: integer;
begin
    cs.Acquire;
    try
        packet := TPacket.create;
        packet.types := TPackType.account;
        packet.action := TAction.Download;

        for i := 0 to acclen - 1 do
        begin
            packet.add(IntToStr(account[i].id));
            packet.add(account[i].user);
            packet.add(account[i].pass);
        end;
        print('Synchronizing [' + IntToStr(acclen) + '] accounts to [' + Context.Binding.PeerIP + ':' +
          IntToStr(Context.Binding.PeerPort) + '].', Magenta);
        Context.Connection.Socket.Write(packet.packetize);
    finally
        cs.Release;
    end;
end;

constructor TAccountDB.create();
begin
    cs := TCriticalSection.create;
    nextid := 1;
    acclen := 0;
end;

function TAccountDB.add(user, pass, serial, ip: string; id: integer = -1): TAccount;
begin
    cs.Acquire;
    try
        account[acclen].user := user;
        account[acclen].pass := pass;
        account[acclen].serial := serial;
        account[acclen].ip := ip;
        account[acclen].date := DateToStr(now);
        account[acclen].online := false;

        if (id = -1) then
            id := nextid;

        account[acclen].id := id;
        inc(nextid);
        inc(acclen);
        result := account[acclen - 1];
    finally
        cs.Release;
    end;
end;

function TAccountDB.license(const serial: string): boolean;
var
    i: integer;
begin
    cs.Acquire;
    try
        result := false;

        for i := 0 to acclen - 1 do
            if (account[i].serial = serial) then
                result := true;
    finally
        cs.Release;
    end;
end;

function TAccountDB.username(const name: string): boolean;
var
    i, len: integer;
begin
    cs.Acquire;
    try
        result := false;

        for i := 0 to acclen - 1 do
            if (account[i].user = name) then
                result := true;
    finally
        cs.Release;
    end;
end;

// check the serial if its VALID !!!!!!!!
function TAccountDB.VerifyInput(var packet, reply: TPacket; license: boolean = true): boolean;
begin
    result := false;
    if (length(packet.param(0)) < USERNAME_LENGTH) or (length(packet.param(0)) > USERNAME_MAXLEN) then
    begin
        reply.add('Username length mismatch ' + IntToStr(USERNAME_LENGTH) + '-' + IntToStr(USERNAME_MAXLEN) +
          ' characters expected.');
        reply.action := TAction.Msg;
    end
    else if (length(packet.param(1)) < PASSWORD_LENGTH) or (length(packet.param(0)) > PASSWORD_MAXLEN) then
    begin
        reply.add('Password length mismatch ' + IntToStr(PASSWORD_LENGTH) + '-' + IntToStr(PASSWORD_MAXLEN) +
          ' characters expected.');
        reply.action := TAction.Msg;
    end
    else if license = true then
    begin
        if ((length(packet.param(2)) < SERIAL_LENGTH) or (length(packet.param(2)) > SERIAL_MAXLEN)) then
        begin
            reply.add('Serial length mismatch!' + Conf_Protocol.NEWLINE + IntToStr(SERIAL_LENGTH) + '-' +
              IntToStr(SERIAL_MAXLEN) + ' characters expected.');
            reply.action := TAction.Msg;
        end
        else
            result := true;
    end
    else
        result := true;
end;

procedure TAccountDB.Registration(var packet: TPacket; var Context: TIdContext);
var
    reply: TPacket;
    registered: boolean;
    i, aID: integer;
    account: TAccount;
    sessid: string[25];
begin
    reply := TPacket.create;
    try
        reply.types := TPackType.Login;
        reply.action := TAction.Msg;

        if (VerifyInput(packet, reply)) then
        begin
            if (AccountDB.license(packet.param(2)) = false) then
            begin
                if (AccountDB.username(packet.param(0)) = false) then
                begin
                    sessid := TTokenManager.generate;
                    reply.action := TAction.registered;
                    reply.add(sessid);

                    account := add(packet.param(0), packet.param(1), packet.param(2), packet.ip);
                    AccountDB.connect(account, Context.Binding.PeerIP, Context.Binding.PeerPort);

                    Context.Connection.Socket.Write(reply.packetize);
                    serverlist.send(Context); // update online count

                    BackEnd.Synchronize(account, Context.Connection.Socket.Binding.PeerIP, sessid);

                    print('Created ' + packet.param(0) + ' @' + Context.Binding.PeerIP + ':' +
                      IntToStr(Context.Binding.PeerPort) + ' {' + packet.param(2) + '}', LightCyan);
                end
                else
                begin
                    reply.add('Username Already In Use.');
                    Context.Connection.Socket.Write(reply.packetize);
                    Context.Connection.disconnect; // disconnect so the client has to reconnect, LIMIT firewall.
                end;
            end
            else
            begin
                reply.add('Serial Key Already Registered.');
                Context.Connection.Socket.Write(reply.packetize);
                Context.Connection.disconnect; // disconnect so the client has to reconnect, LIMIT firewall.
            end;
        end
        else
            Context.Connection.Socket.Write(reply.packetize);
    finally
        reply.free;
    end;
end;

function TAccountDB.Authenticate(const user: string; const pass: string; var account: Pointer;
  sender: String = 'Meta'): boolean;
var
    i: integer;
begin
    result := false;
    cs.Acquire;
    try
        for i := 0 to acclen - 1 do
        begin
            if (self.account[i].pass = pass) and (self.account[i].user = user) then
            begin
                account := @self.account[i];
                result := true;
                // print('World Authentication [' + TAccount(account^).user + '].', white);
            end;
        end;
    finally
        cs.Release;
    end;
    if result = false then
        print(sender + ':Authentication Failure {' + user + ', ' + pass + '}', LightRed);
end;

// check if the user is banned, implement login queue.
procedure TAccountDB.Login(var packet: TPacket; var Context: TIdContext; const user, pass: string);
var
    reply: TPacket;
    sessid: string[25];
begin
    reply := TPacket.create;
    try
        if VerifyInput(packet, reply, false) = true then
        begin
            if (Authenticate(user, pass, Pointer(TSession(Context.data).account))) then
            begin
                { if (TSession(Context.data).account.online) and (false) then // already logged on
                  begin
                  reply.types := TPackType.Login;
                  reply.action := TAction.Msg;
                  reply.add('Error: Already Logged On.');
                  Context.Connection.Socket.WriteLn(reply.packetize, , packet.Encoding);
                  end
                  else // not logged on, login
                  begin }
                sessid := TTokenManager.generate;

                connect(TSession(Context.data).account^, Context.Binding.PeerIP, Context.Binding.PeerPort);
                reply.types := TPackType.Login;
                reply.action := TAction.Success; // change this to serverselect
                reply.add(sessid); // session id
                Context.Connection.Socket.Write(reply.packetize);
                serverlist.send(Context);

                BackEnd.Synchronize(TSession(Context.data).account^, Context.Connection.Socket.Binding.PeerIP, sessid);
                { end; }
            end
            else
            begin // login failed
                reply.types := TPackType.Login;
                reply.action := TAction.Failure;
                Context.Connection.Socket.Write(reply.packetize);
                Context.Connection.disconnect;
            end;
        end
        else
        begin // invalid format
            reply.types := TPackType.Login;
            reply.action := TAction.Msg;
            Context.Connection.Socket.Write(reply.packetize);
        end;
    finally
        reply.free;
    end;
end;

function TAccountDB.position(var stack: array of TAccount; id: integer): integer;
var
    i, len: integer;
begin
    cs.Acquire;
    try
        result := -1;

        for i := 0 to acclen - 1 do
            if (stack[i].id = id) then
                result := i;
    finally
        cs.Release;
    end;
end;

function TAccountDB.registered(const serial: string): boolean;
var
    i: integer;
begin
    cs.Acquire;
    try
        result := false;

        for i := 0 to acclen - 1 do
            if (self.account[i].serial = serial) then
                result := true;
    finally
        cs.Release;
    end;
end;

function TAccountDB.loggedon(id: integer): boolean;
begin
    if account[position(account, id)].online then
        result := true
    else
        result := false;
end;

function TAccountDB.connect(var account: TAccount; const ip: string; const port: word): boolean;
begin
    result := false;

    if account.online then
    begin
        print('Already Connected #' + IntToStr(account.id) + ' ' + account.user, Red);
    end
    else if (acclen < MAXACCOUNT) then
    begin
        account.online := true;
        print('Account Login: #' + IntToStr(account.id) + ' [' + account.user + '] From [' + ip + ':' + IntToStr(port) +
          ']', Cyan);
        result := true;
    end
    else
        print('Server Full #' + IntToStr(account.id) + ' ' + account.user, LightRed);
end;

procedure TAccountDB.disconnect(var account: TAccount);
begin
    if account.online = true then
    begin
        print('Disconnected #' + IntToStr(account.id) + ' ' + account.user, DarkGray);
        account.online := false;
    end
    else
        print('Not Disconnected, No Connection #' + IntToStr(account.id) + ' ' + account.user, LightRed);
end;

procedure TAccountDB.load();
var
    savefile: file of TAccount;
begin
    acclen := 0;
    print('Reading Account Data..', Brown);
    try
        if not(fileexists('data/account.data')) then
        begin
            print(#9 + ' No Data.', LightRed, false);
            exit;
        end;

        assignFile(savefile, 'data/' + 'account.data');
        reset(savefile);

        while not eof(savefile) do
        begin
            account[acclen].online := false;
            read(savefile, account[acclen]);
            inc(acclen);
        end;

        self.nextid := account[acclen - 1].id + 1;
        closefile(savefile);
        print(#9 + ' Done.', LightGreen, false);
    except
        print(#9 + ' Not Loaded.', Red, false);
    end;
end;

procedure TAccountDB.save;
var
    savefile: file of TAccount;
    i, len: integer;
begin
    cs.Acquire;
    try
        print('');
        print('Saving Accounts..', Brown);

        assignFile(savefile, 'data/' + 'account.data');
        rewrite(savefile);

        for i := 0 to acclen - 1 do
        begin
            write(savefile, self.account[i]);
        end;

        closefile(savefile);

        print(' Done.', LightGreen, false);
    finally
        cs.Release;
    end;
end;

procedure initialize();
begin
    AccountDB := TAccountDB.create();
    AccountDB.load;
end;

begin

end.
