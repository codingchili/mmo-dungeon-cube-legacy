unit System_Audio;

interface

uses {$IFDEF Win32}MMSystem, {$ENDIF} Classes, SysUtils, System_UtilExt
{$IFDEF Win32}, AsphyreSound, DirectSound{$ENDIF};

type
  TMusic = class
  public
    trackID: integer;
    ambient: boolean;
    filename: TStringList;
    {$IFDEF Win32}
    device: TAsphyreSound;
    {$ENDIF}
    constructor Create(FileMan: TFileControl);
    function isPlaying(): boolean;
    procedure Play(Name: string; ambient: boolean = False); overload;
    procedure Play(); overload;
    procedure Stop();
  end;

type
  TSoundMaster = class(TThread)
  protected
    procedure Execute; override;
  public
    procedure Play(Name: string; ambient: boolean = False);
  private
    music: array of TMusic;
    constructor Create();
  end;

procedure Initialize(nosound: boolean);

var
  SoundMaster: TSoundMaster;

implementation

uses Main, System_Log, System_Initializer;

procedure TMusic.Play();
begin
  {$IFDEF Win32}
  device.Sound[trackID].Play();
  {$ENDIF}
end;

procedure TMusic.Stop;
begin
  ambient := False;
  {$IFDEF Win32}
  device.Sound[trackID].Reset;
  device.Sound[trackID].Stop;
  {$ENDIF}
end;

function TMusic.isPlaying;
begin
  {$IFDEF Win32}
  Result := device.Sound[trackID].IsSoundPlaying;
    {$ENDIF}
end;

procedure TMusic.Play(Name: string; ambient: boolean = False);
var
  i: integer;
begin
  {$IFDEF Win32}
  for i := 0 to high(device.Sound) do
    if (filename.Strings[i] = Name) then
    begin
      trackID := i;
      self.ambient := ambient;
      device.Sound[i].Play();
    end;
  {$ENDIF}
end;

constructor TMusic.Create(FileMan: TFileControl);
var
  i: integer;
begin
  {$IFDEF Win32}
  device := TAsphyreSound.Create;

  if not device.Initialize(Main.RenderForm.Handle) = True then
  begin
    print('Error: Failed To Initialize Sound.');
  end;
    {$ENDIF}

  filename := TStringList.Create;

  {$IFDEF Win32}
  for i := 0 to High(FileMan.Files) do
  begin
    filename.Add(FileMan.Files[i].filename);
    device.AddSoundFromFile(FileMan.Files[i].filepath + FileMan.Files[i].filename);
  end;
  {$ENDIF}

  ambient := False;
end;

procedure TSoundMaster.Execute;
var
  i: integer;
begin
  try
    while True do
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
procedure TSoundMaster.Play(Name: string; ambient: boolean = False);
var
  i: integer;
begin
  if not (Initializer.nosound) then
  begin
    Name := Name + '.wav';

    if (ambient) then
      for i := 0 to high(music) do
        if (self.music[i].ambient) then
          self.music[i].Stop();

    for i := 0 to high(music) do
    begin
      if not (self.music[i].ambient) and not (self.music[i].isPlaying) then
      begin
        print(Name + ' assigned to device #' + IntToStr(i));
        music[i].Play(Name, ambient);
        exit;
      end;
    end;
  end;
end;

constructor TSoundMaster.Create;
var
  FileMan: TFileControl;
  i: integer;
begin
  inherited Create(false);
  SetLength(music, 3);
  FileMan := TFileControl.Create;

  {$IFDEF Win32}
  FileMan.FileSearch('media\sound\', '*.wav', False);
  print('Loading Sound ..');

  for i := 0 to high(music) do
    music[i] := TMusic.Create(FileMan);
    {$ENDIF}

  FileMan.Free;
end;

procedure Initialize(nosound: boolean);
begin
  SoundMaster := TSoundMaster.Create;

  if not (nosound) then
    SoundMaster.Play('background', True);
end;

end.