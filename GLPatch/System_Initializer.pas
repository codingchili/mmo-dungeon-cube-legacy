unit System_Initializer;

interface

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

uses
  {$IFDEF Win32} Windows, vcl.forms, {$ENDIF}
  {$IFDEF Linux} Forms, Graphics, LResources, {$ENDIF}
  SysUtils, Vectors2px, AbstractDevices, AbstractCanvas, AsphyreImages,
  AsphyreFonts, AsphyreArchives, classes, Dialogs;

Const
  PARAMS: array [0 .. 10] of string = ('-version', '-patch', '-license',
    '-user', '-pass', '-serial', '-dev', '-logger', '-packet', '-help',
    '-ping');

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
    version: string;
    constructor Create();
  end;

procedure initialize;

var
  DisplaySize: TPoint2px;
  GameDevice: TAsphyreDevice = nil;
  GameCanvas: TAsphyreCanvas = nil;
  GameImages: TAsphyreImages = nil;
  GameFonts: TAsphyreFonts = nil;
  Initializer: TInitializer;

implementation

uses Main,
  GUI_Engine,
  System_UtilExt,
 System_Log, System_Text,
  Conf_Protocol, Effects_Fire, Debug_Stopwatch, System_Keyboard, System_Mouse, Engine_Particles, Effects_Lightning;

procedure TInitializer.PrintParameters();
begin
  print('This is the help, you have either entered a misformed parameter or issued -help.',
    magenta);
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
  print(#13 + #10 + #9 + '-help' + #9 + #9 + 'Display this help.', cyan);
end;

procedure initialize;
begin
  Initializer := TInitializer.Create;
end;

function TInitializer.ParamIndex(str: string): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to high(PARAMS) do
    if (str = PARAMS[i]) then
      result := i;
end;

procedure TInitializer.ExtractParams();
var
  i: integer;
  key, value: string;
  paramlist: TStringList;
begin
  System_UtilExt.root := ExtractFilePath(ParamStr(0));
  update := false;
  license := false;
  developer := false;

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

    if paramlist.count > 0 then
      key := paramlist[0];
    if paramlist.count > 1 then
      value := paramlist[1];

    case ParamIndex(key) of
      0:
        version := value;
      1:
        update := true;
      2:
        license := true;
      3:
        Protocol.user := value;
      4:
        Protocol.pass := value;
      5:
        Protocol.serial := value;
      6:
        developer := true;
      7:
        System_Log.logging := true;
      8:
        Protocol.logging := true;
      9:
        begin
          System_Log.logging := true;
          PrintParameters();
          halt;
        end;
      10:
        ping := true;
      -1:
        begin
          System_Log.logging := true;
          PrintParameters();
          showmessage('Please, Run Launcher.exe!');
          print('');
          halt;
        end;
    end;
  end;

  paramlist.Free;
end;

constructor TInitializer.Create;
var
  sw: TStopwatch;
begin
  {$IFDEF Win32}
  application.ProcessMessages;
  {$ENDIF}
  sw := TStopwatch.Create(true);
  Protocol := TProtocol.Create;

  System_Log.initialize;
  ExtractParams();

  swidth := main.RenderForm.Width;
  sheight := main.RenderForm.Height;

  {$IFDEF Win32}
  Screen.Cursors[0] := LoadCursor(HInstance, 'MAINCUR');
  {$ELSE}
  Screen.Cursors[0] := LoadCursorFromLazarusResource('main');
  {$ENDIF}
  Screen.Cursor := Screen.Cursors[0];

  System_Log.initialize;
  print('Initializing Input..');
  System_Keyboard.initialize;
  System_Mouse.initialize;
  print('Initializing Particles..');
  Engine_Particles.initialize;
  print('Initializing Effects..');
  Effects_Lightning.initialize;
  Effects_Fire.initialize;
  print('Initializing Text..');
  System_Text.initialize;

  sw.Stop;
  sw.Free;

  print('Loaded in ' + sw.Elapsed);
end;

begin
  {$IFDEF Linux}{$I mcursor.lrs}{$ENDIF}
END.