unit Network_Patching;

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

interface

uses SysUtils, Network_MetaServer, IdContext, Conf_Protocol, System_UtilExt,
    Conf_Patching, Classes, System_Log, SyncObjs, IdTCPServer, idGlobal,
    IdSockethandle;

const
    PORT = 1576;

type
    TUpload = class(TThread)
    protected
        procedure Execute; override;
        constructor Create(var Context: TIdContext; var Data: TBytes; filesize: int64);
    public
        filesize: int64;
        filename: string;
        Context: TIdContext;
        Data: ^TIdBytes;
    private
    end;

type
    TPatchServer = class
    public
        constructor Create();
    private
        Server: TIdTCPServer;
        class procedure OnConnected(Acontext: TIdContext);
        class procedure OnRead(Acontext: TIdContext);
        class procedure sizepacket(var Packet: TPacket; var Acontext: TIdContext);
        class procedure upload(var Packet: TPacket; var Context: TIdContext);
    end;

procedure initialize;

var
    PatchServer: TPatchServer;

implementation

constructor TPatchServer.Create;
begin
    Server := TIdTCPServer.Create();

    Print('Opening Inbound Patch..', Brown);
    Server.ReuseSocket := rsTrue;

    with Server.bindings.add do
    begin
        IPVersion := ID_Ipv4;
        ip := '';
        PORT := Network_Patching.PORT;
    end;

    Server.OnExecute := self.OnRead;
    Server.OnConnect := self.OnConnected;
    try
        Server.Active := true;
    except
        On E: Exception do
        begin
            Print('Failed to start Patching Service! ' + E.message, System_Log.Red);
            halt;
        end;
    end;
    Print(#9 + ' Done. [:' + inttostr(Network_Patching.PORT) + ']', lightgreen, false);
end;

class procedure TPatchServer.OnConnected(Acontext: TIdContext);
begin
    Acontext.Connection.Socket.RecvBufferSize := 512;
end;

class procedure TPatchServer.OnRead(Acontext: TIdContext);
var
    Packet: TPacket;
    Data: TIdBytes;
    len: word;
begin
    Packet := TPacket.Create;
    try
        Packet.ip := Acontext.Connection.Socket.Binding.PeerIP;
        Packet.PORT := Acontext.Connection.Socket.Binding.PeerPort;

        // get message length
        Acontext.Connection.Socket.ReadBytes(Data, 2);
        Move(Data[0], len, SizeOf(len));

        // verify packet size.
        if (len > Conf_Protocol.MAXBYTES) or (len < 0) then
        begin
            // drop all bytes in buffer.
            Acontext.Connection.Socket.InputBuffer.Clear;
            // Acontext.Connection.Socket.DiscardAll;
        end
        else
        begin

            // reset buffer
            SetLength(Data, 0);

            // get message
            Acontext.Connection.Socket.ReadBytes(Data, len);

            // parse and unpack
            Packet.unpacketize(BytesToString(Data, IndyTextEncoding(Packet.Encoding)));

            case (Packet.action) of
                TAction.Version:
                    Acontext.Connection.Socket.Write(FloatToStr(configs.Version) + 'Z');
                TAction.filesize:
                    sizepacket(Packet, Acontext);
                TAction.FileData:
                    upload(Packet, Acontext);
            end;

        end;
    finally
        Packet.free;
    end;
end;

constructor TUpload.Create(var Context: TIdContext; var Data: TBytes; filesize: int64);
begin
    inherited Create(false);
    self.OnTerminate := OnTerminate;
    // FreeOnterminate := true;
    self.Context := Context;
    self.filesize := filesize;
    self.Data := @Data;
    // Resume;
end;

procedure TUpload.Execute;
begin
    try
        Context.Connection.Socket.Write(self.Data^, self.filesize);
    except
        on E: Exception do
            Print(E.message);
    end;
end;

class procedure TPatchServer.sizepacket(var Packet: TPacket; var Acontext: TIdContext);
var
    i: integer;
    filelist: TPatchList;
    reply: TPacket;
begin
    filelist := configs.metadata(SafeFloat(Packet.param(0)));
    reply := TPacket.Create();
    try
        reply.add(inttostr(length(filelist)));

        for i := 0 to length(filelist) - 1 do
        begin
            reply.add(filelist[i].name);
            reply.add(FloatToStr(filelist[i].Version));
            reply.add(inttostr(filelist[i].size));
            reply.add(inttostr(filelist[i].index));
        end;

        Acontext.Connection.Socket.Write(reply.packetize);
    finally
        reply.free;
    end;
end;

class procedure TPatchServer.upload(var Packet: TPacket; var Context: TIdContext);
var
    index: integer;
    uploader: TUpload;
begin
    index := StrToInt(Packet.param(0));

    if (index > -1) and (index < length(configs.files)) then
        uploader := TUpload.Create(Context, configs.files[index].bytes, configs.files[index].size)
    else
        Context.Connection.Disconnect; // messing are you not.
end;

procedure initialize;
begin
    PatchServer := TPatchServer.Create;
end;

end.
