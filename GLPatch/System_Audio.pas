unit System_Audio;

// todo: add linux support for System_Audio.

interface

uses {$IFDEF Win32}MMSystem, {$ENDIF} Classes, SysUtils, System_UtilExt
{$IFDEF Win32}, AsphyreSound, DirectSound{$ENDIF};

type
  TMusic = class
  public
    trackID: integer;
    ambient: boolean;
    filename: TStringList;
    device: TAsphyreSound;
    constructor create(FileMan: TFileControl);
    function isPlaying(): boolean;
    procedure Play(name: string; ambient: boolean = false); overload;
    procedure Play(); overload;
  end;

type
  TSoundMaster = class(TThread)
  protected
    procedure Execute; override;
  public
    procedure Play(name: string; ambient: boolean = false);
  private
    music: array of TMusic;
    constructor create;
  end;

procedure Initialize;

var
  SoundMaster: TSoundMaster;

implementation

uses Main, System_Log;

procedure TMusic.Play();
begin
  device.Sound[trackID].Play();
end;

function TMusic.isPlaying;
begin
  if (length(device.Sound) > trackID) and (trackID > 0) then
    result := device.Sound[trackID].IsSoundPlaying
  else
    result := false;
end;

procedure TMusic.Play(name: string; ambient: boolean = false);
var
  i: integer;
begin
  for i := 0 to high(device.Sound) do
    if (filename.Strings[i] = name) then
    begin
      trackID := i;
      self.ambient := ambient;
      device.Sound[i].Play();
    end;
end;

constructor TMusic.create(FileMan: TFileControl);
var
  i: integer;
begin
  device := TAsphyreSound.create;
  filename := TStringList.create;

  if not device.Initialize(Main.RenderForm.Handle) = true then
  begin
    print('Error: Failed To Initialize Sound.');
  end;

  for i := 0 to High(FileMan.Files) do
  begin
    filename.Add(FileMan.Files[i].filename);
    device.AddSoundFromFile(FileMan.Files[i].filepath + FileMan.Files[i].filename);
  end;

  ambient := false;
end;

procedure TSoundMaster.Execute;
var
  i: integer;
begin
  try
    while true do
    begin
      for i := 0 to high(self.music) do
        if music[i].ambient and not music[i].isPlaying then
          music[i].Play();

      sleep(24);
    end;
  except
    on E: Exception do
      print(E.Message);
  end;
end;

// get resource from name and send to thread which will insert[] to its array. 0 element!!!
procedure TSoundMaster.Play(name: string; ambient: boolean = false);
var
  i: integer;
begin
  name := name + '.wav';

  for i := 0 to high(music) do
  begin
    if not(self.music[i].ambient) and not(self.music[i].isPlaying) then
    begin
      print(name + ' assigned to device #' + inttostr(i));
      music[i].Play(name, ambient);
      exit;
    end;
  end;
end;

constructor TSoundMaster.create;
var
  i: integer;
  FileMan: TFileControl;
begin
  inherited create;
  SetLength(music, 1);
  FileMan := TFileControl.create;
  // FileMan.FileSearch('media\sound\', 'hover.wav', false);
  // FileMan.FileSearch('media\sound\', 'click.wav', false);
  // FileMan.Load();
  // print('Loading Sound ..');

  for i := 0 to high(music) do
    music[i] := TMusic.create(FileMan);

  FileMan.Free;
end;

procedure Initialize;
begin
  SoundMaster := TSoundMaster.create;
  // SoundMaster.Play('background', true);
  // SoundMaster.Play('stream', true);
end;

end.
