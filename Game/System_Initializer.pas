unit System_Initializer;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
{$IFDEF Win32} Windows, vcl.Forms, {$ENDIF}
{$IFDEF Linux} Forms, Graphics, LResources, {$ENDIF}
  SysUtils, Vectors2px, AbstractDevices, AbstractCanvas, AsphyreImages,
  AsphyreFonts, AsphyreArchives, Classes, Debug_Stopwatch, Dialogs;

const
  PARAMS: array [0 .. 12] of string =
    ('-version', '-patch', '-license', '-user', '-pass', '-serial', '-dev', '-logger',
    '-packet', '-help', '-ping', '-window', '-nosound');

type
  TInitializer = class
  private
    function ParamIndex(str: string): integer;
    procedure ExtractParams();
    procedure PrintParameters();
  public
    swidth, sheight: integer;
    update: boolean;
    license: boolean;
    developer: boolean;
    ping: boolean;
    windowed: boolean;
    nosound: boolean;
    version: string;
    procedure Run();
  end;

procedure Initialize;

var
  DisplaySize: TPoint2px;
  GameDevice: TAsphyreDevice = nil;
  GameCanvas: TAsphyreCanvas = nil;
  GameImages: TAsphyreImages = nil;
  GameFonts: TAsphyreFonts = nil;
  Initializer: TInitializer;

implementation

uses Main, System_Camera, System_Keyboard,
  GUI_CharSelect, GUI_Engine,
  System_UtilExt, Network_MetaServer, GUI_LoginScreen, Engine_Particles,
  GUI_Bars, Engine_Map, Engine_Projectiles, System_Ping,
 {$IFDEF Win32}System_Audio, {$ENDIF}
  Engine_Player, Network_World, System_Multiplayer, System_Text, System_Log,
  Conf_Protocol, Network_WorldServer, Effects_Lightning, Effects_Fire,
  System_Spells, Effects_Splat, System_Mouse, System_DayTime,
  Effects_Smoke, Effects_Water;

procedure TInitializer.PrintParameters();
begin
  print('This is the help, you have either entered a misformed parameter or issued -help.', magenta);
  print(#9 + '-version:string' + #9 + 'Set the running version.', cyan);
  print(#9 + '-patch' + #9 + #9 + 'Show the patch notes.', cyan);
  print(#9 + '-license' + #9 + 'Show the ToU/License.', cyan);
  print(#9 + '-user:string' + #9 + 'Preset the username.', cyan);
  print(#9 + '-pass:string' + #9 + 'Preset the password.', cyan);
  print(#9 + '-serial:string' + #9 + 'Preset the serial key.', cyan);
  print(#9 + '-dev' + #9 + #9 +
    'Shows debug info and enables in-game console ALT + C.', cyan);
  print(#9 + '-logger' + #9 + #9 + 'Enable the Event Logger.', cyan);
  print(#9 + '-packet' + #9 + #9 + 'Enable the Packet Logger.', cyan);
  print(#9 + '-ping' + #9 + #9 + 'Enable pinger, use with -dev.', cyan);
  print(#9 + '-nosound ' + #9 + 'Disable all sounds.', cyan);
  print(#9 + '-window:x|y' + #9 + 'Run in windowed mode. [DISABLED]', cyan);
  print(#13 + #10 + #9 + '-help' + #9 + #9 + 'Display this help.', cyan);
end;

procedure Initialize;
begin
  Initializer := TInitializer.Create;
  Initializer.Run();
end;

function TInitializer.ParamIndex(str: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to high(PARAMS) do
    if (str = PARAMS[i]) then
      Result := i;
end;

procedure TInitializer.ExtractParams();
var
  i: integer;
  key, Value: string;
  paramlist: TStringList;
begin
  System_UtilExt.root := ExtractFilePath(ParamStr(0));
  update := False;
  license := False;
  developer := False;
  nosound := True;

  if paramcount = 0 then
  begin
    // System_Log.logging := true;
    // PrintParameters();
    // showmessage('Please, Run Launcher.exe!');
    // halt;
  end;

  paramlist := TStringList.Create;

  for i := 1 to paramcount do
  begin
    Split(':', ParamStr(i), paramlist);

    if paramlist.Count > 0 then
      key := paramlist[0];
    if paramlist.Count > 1 then
      Value := paramlist[1];

    case ParamIndex(key) of
      0:
        version := Value;
      1:
        update := True;
      2:
        license := True;
      3:
        Protocol.user := Value;
      4:
        Protocol.pass := Value;
      5:
        Protocol.serial := Value;
      6:
        developer := True;
      7:
        System_Log.logging := True;
      8:
        Protocol.logging := True;
      9:
      begin
        System_Log.logging := True;
        PrintParameters();
        halt;
      end;
      10:
        ping := True;
      11:
        windowed := True;
      12:
        nosound := True;
      -1:
      begin
        System_Log.logging := True;
        PrintParameters();
        ShowMessage('Please, Run Launcher.exe!');
        print('');
        halt;
      end;
    end;
  end;

  paramlist.Free;
end;

procedure TInitializer.Run;
var
  sw: TStopwatch;
begin
  sw := TStopwatch.Create(True);
  Protocol := TProtocol.Create;

  windowed := False;
  System_Log.Initialize;
  ExtractParams();

{$IFDEF Win32}
  System_Audio.Initialize(nosound);
{$ENDIF}
  swidth := Main.RenderForm.Width;
  sheight := Main.RenderForm.Height;

{$IFDEF Win32}
  Screen.Cursors[0] := LoadCursor(HInstance, 'MAINCUR');
{$ELSE}
  Screen.Cursors[0] := LoadCursorFromLazarusResource('main');
{$ENDIF}
  Screen.Cursor := Screen.Cursors[0];

  Randomize; // todo use a stronger seed
  System_Log.Initialize;
  print('Initializing Input..');
  System_Keyboard.Initialize;
  System_Mouse.Initialize;
  print('Initializing Networking..');
  Network_World.Initialize;
  Network_WorldServer.Initialize;
  Network_MetaServer.Initialize;
  print('Initializing Player..');
  Player := TPlayer.Create;
  print('Initializing Camera..');
  System_Camera.Initialize;
  print('Initializing Projectiles..');
  Engine_Projectiles.Initialize;
  print('Initializing Spells..');
  System_Spells.Initialize;
  print('Initializing Particles..');
  Engine_Particles.Initialize;
  print('Initializing Effects..');
  Effects_Lightning.Initialize;
  Effects_Fire.Initialize;
  Effects_Splat.Initialize;
  Effects_Smoke.Initialize;
  Effects_Water.Initialize;
  print('Initializing Multiplayer..');
  System_Multiplayer.Initialize;
  print('Initializing Text..');
  System_Text.Initialize;
  print('Initializing Time..');
  System_DayTime.Initialize;

  sw.Stop;

  if (self.ping) then
  begin
    print('Initializing Ping..');
    System_Ping.Initialize;
  end;
  print('Loaded in ' + sw.Elapsed);
  sw.Free;
end;

begin
{$IFDEF Linux}{$I mcursor.lrs}{$ENDIF}

end.