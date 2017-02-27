unit Network_MetaServer;

// tcp server in passive mode.
{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses Classes, SysUtils, Dialogs, idSocketHandle,
  IdTCPClient, Graphics, {$IFDEF Win32}Windows, VCL.controls, WinSock, {$ENDIF} Conf_Protocol,
  Effects_Blending, SyncObjs, Debug_StopWatch, IdGlobal;

// the listener should be in an thread, not the entire object!

type
  TConnectionListener = class(TThread)
  protected
    procedure Execute; override;
    constructor create(clientptr, csptr: Pointer);
  private
    client: ^TIdTCPClient;
    cs: ^TCriticalSection;
  end;

type
  TOnConnected = procedure of object;

  // update the gui while we connect, when connected trigger OnConnected in TMetaCon. (fncptr?)
type
  TAsyncConnect = class(TThread)
  protected
    procedure Execute; override;
  private
    client: ^TIdTCPClient;
    cs: ^TCriticalSection;
    OnConnected: TOnConnected;
  public
    constructor create(clientptr: Pointer; OnConnected: TOnConnected; cs: Pointer);
  end;

type
  TMetaCon = class
  protected
    // procedure Execute; override;
  private
    cs: TCriticalSection;
    license, username, password: string;
    AsyncConnect: TAsyncConnect;
    listener: TConnectionListener;
    client: TIdTCPClient;
    sw: TStopWatch;
    pingtime: integer; // does this connection really need pinger?
  public
    constructor create();
    procedure connect(OnConnected: TOnConnected);
    procedure ListServers();
    procedure LoginReply(packet: TPacket);
    procedure PingResponse();
    procedure login(username, password: string; license: string = ''); overload;
    procedure login(); overload;
    procedure WorldServer(packet: TPacket);
    procedure Ping(sessid: string; roundtrip: boolean = false);
    procedure Disconnect();
    procedure syncconnect();
    function connected(): boolean;
  end;

procedure Initialize;

var
  MetaCon: TMetaCon;

implementation

uses System_Initializer, GUI_LoginScreen, GUI_ServerSelect, Main, GUI_StatusBox,
  System_Log, GUI_SplashMain;

procedure Initialize;
begin
  MetaCon := TMetaCon.create;
end;

function TMetaCon.connected;
begin
  result := client.connected;
end;

procedure TMetaCon.Disconnect;
begin
  cs.Acquire;
  try
    client.Socket.Close;
  finally
    cs.Release;
  end;
end;

procedure TMetaCon.syncconnect();
begin
  cs.Acquire;
  client.ReadTimeout := 300;
  client.ConnectTimeout := 300;
  try
    client.connect();
  except
  end;
  cs.Release;
end;

constructor TAsyncConnect.create(clientptr: Pointer; OnConnected: TOnConnected; cs: Pointer);
begin
  inherited create(false);
  self.client := clientptr;
  self.cs := cs;
  self.OnConnected := OnConnected;
  self.cs^.Acquire;
end;

procedure TAsyncConnect.Execute;
var
  attempt: integer;
begin
  attempt := 1;

  client^.ReadTimeout := 600;
  client^.ConnectTimeout := 600;
  statusbox.settext('Connecting ..');
  statusbox.show;
  try
    try
      client^.Disconnect;
    except
      on E: Exception do
        print(E.Message + ' In Meta1:TAsyncConnect()');
    end;

    try
      client^.connect;
    except
      on E: Exception do
        print(E.Message + ' In Meta2:TAsyncConnect()');
    end;

    while (client^.connected = false) and (attempt < 4) do
    begin
      statusbox.settext('Reconnecting [' + inttostr(attempt) + '/' + inttostr(3) + '] ..');
      client^.ReadTimeout := client^.ReadTimeout + 425;
      client^.ConnectTimeout := client^.ConnectTimeout + 425;

      try
        client^.connect;
      except
        on E: Exception do
          print(E.Message + ' In Meta3:TAsyncConnect()');
      end;
      inc(attempt);
    end;

    if (client^.connected = false) then
    begin
      statusbox.settext('Could Not Connect.');
      statusbox.finished;
      LoginScreen.SetView();
    end;

  finally
    cs^.Release;
  end;

  // call this last, prevents from resuming the logic when the state might have been changed.
  if (client^.connected) then
  begin
    if (@OnConnected <> NIL) then
      Synchronize(self, self.OnConnected);
  end;
end;

constructor TConnectionListener.create(clientptr: Pointer; csptr: Pointer);
begin
  inherited create(false);
  client := clientptr;
  cs := csptr;
end;

constructor TMetaCon.create();
begin
  inherited create;
  sw := TStopWatch.create();
  cs := TCriticalSection.create;
  client := TIdTCPClient.create();
  client.port := Protocol.port;
  client.host := Protocol.host;

{$IFDEF Win32}
  client.UseNagle := false;
{$ENDIF}
  client.ConnectTimeout := 600;
  client.ReadTimeout := 600;
  listener := TConnectionListener.create(@client, @cs);
end;

procedure TMetaCon.WorldServer(packet: TPacket);
begin
  ServerScreen.AddServer(packet.param(0), packet.param(1), packet.param(2), StrToInt(packet.param(3)),
    StrToInt(packet.param(4)), StrToInt(packet.param(5)), packet.param(6), packet.param(7));
end;

procedure TMetaCon.Ping(sessid: string; roundtrip: boolean = false);
var
  packet: TPacket;
begin
  packet := TPacket.create;
  cs.Acquire;
  try
    packet.types := TPackType.Ping;
    packet.action := TAction.None;
    sw.Start;
    client.Socket.Write(packet.packetize);
  finally
    cs.Release;
    packet.free;
  end;
end;

procedure TMetaCon.ListServers();
var
  packet: TPacket;
  connected: boolean;
begin
  packet := TPacket.create;
  connected := true;
  cs.Acquire;
  try
    packet.types := TPackType.Serverlist;
    packet.action := TAction.Info;

    statusbox.show('Retrieving Servers ..');

    client.Socket.Write(packet.packetize);
    // is threadsafe.
    // cs.Release;
  except
    // statusbox.hide;
    connected := false;
  end;
  packet.free;
  cs.Release;

  if not(connected) then
    self.connect(self.ListServers);
  // notice: spawns a thread, this function needs to be threadsafe.
end;

procedure TMetaCon.LoginReply(packet: TPacket);
begin
  if packet.action = TAction.msg then
  begin
    statusbox.settext(packet.param(0));
    statusbox.show;
    statusbox.finished;
  end;

  if packet.action = TAction.Failure then
  begin
    statusbox.settext('Could Not Authenticate.');
    statusbox.show;
    statusbox.finished;
    LoginScreen.passfield.text := '';
  end;

  if (packet.action = TAction.Success) or (packet.action = TAction.Registered) then
  begin
    Protocol.sessid := packet.param(0);
    statusbox.show('Retrieving Servers..');
    ServerScreen.SetView;
  end;
end;

procedure TMetaCon.login(username: string; password: string; license: string = '');
begin
  self.username := username;
  self.password := password;
  self.license := license;
  login();
end;

procedure TMetaCon.login();
var
  packet: TPacket;
  connected: boolean;
begin
  connected := false;
  packet := TPacket.create;
  try
    packet.types := TPackType.login;
    packet.action := TAction.None;
    packet.Add(username);
    packet.Add(password);

    if (license <> '') then
    begin
      packet.action := TAction.Registered;
      packet.Add(license);
    end;

    statusbox.show;

    statusbox.settext('Logging In..');
    client.Socket.Write(packet.packetize);
    connected := true;
  except
    connected := false;
  end;
  packet.free;

  print('@login(): connected = ' + BoolToStr(connected, true));

  if (connected = false) then
    self.connect(self.login);
end;

procedure TMetaCon.PingResponse;
begin
  sw.Stop;
  pingtime := sw.ElapsedTicks;
end;

procedure TMetaCon.connect(OnConnected: TOnConnected);
begin
  AsyncConnect := TAsyncConnect.create(@client, OnConnected, @cs);
  listener := TConnectionListener.create(@client, @cs);
end;

procedure TConnectionListener.Execute;
var
  packet: TPacket;
  data: TIdBytes;
  len: word;
begin
  self.FreeOnTerminate := true;
  packet := TPacket.create;
  while not(terminated) do
  begin
    sleep(1);

    cs^.Acquire;
    try
      if (client^.connected) then
      begin
        while (client^.Socket.InputBuffer.Size > 0) do
        begin
          // get message length
          client.Socket.ReadBytes(data, 2);
          Move(data[0], len, SizeOf(len));

          // verify packet size.
          if (len > Conf_Protocol.MAXBYTES) or (len < 0) then
          begin
            // drop all bytes in buffer.
            client.Socket.InputBuffer.Clear;
            client.Socket.DiscardAll;
            SetLength(data, 0);
          end
          else
          begin
            // reset buffer
            SetLength(data, 0);

            // get message
            client.Socket.ReadBytes(data, len);

            // parse and unpack
            packet.unpacketize(BytesToString(data, IndyTextEncoding(packet.Encoding())));
            SetLength(data, 0);

            case (packet.types) of
              TPackType.Ping:
                MetaCon.PingResponse();
              TPackType.login:
                MetaCon.LoginReply(packet);
              TPackType.Serverlist:
                case (packet.action) of
                  TAction.Update:
                    MetaCon.WorldServer(packet);
                  TAction.None:
                    ServerScreen.NullServer;
                end;
            end;
          end;
        end;
      end;
    finally
      // socket error, ignore
      cs^.Release;
    end;
  end;
end;

end.