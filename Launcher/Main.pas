unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, ShellApi, IdGlobal, IdTCPClient,
  IdSocketHandle, IdComponent, SyncObjs, Stopwatch;

type
  TpForm = class(TForm)
    frame: TImage;
    bar1unl: TImage;
    bar2unl: TImage;
    bar1ld: TImage;
    bar2ld: TImage;
    Timer1: TTimer;
    transferred: TLabel;
    status: TLabel;
    transferspeed: TLabel;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure frameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
  public
    cs: TCriticalSection;
    { Public declarations }
  end;

type
  TNetWorkerGUI = class
  public
    class procedure IdTCPClient1Work(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure IdTCPClient1WorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdTCPClient1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  private
  end;

procedure processmessages;

var
  pForm: TpForm;
  mainpageloaded, errorlevel: boolean;
  netGUI: TNetWorkerGUI;
  tick, lastwork, starttick: Int64;
  sw: TStopWatch;
  noexecute: boolean;
  urlmask: string;

implementation

Uses System_Vars, Patching;

{$R *.dfm}

procedure SetTransparent(Aform: TForm; AValue: boolean);
begin
  pForm.TransparentColor := AValue;
  pForm.TransparentColorValue := pForm.Color;
end;

procedure TpForm.FormCreate(Sender: TObject);
begin
  cs := TCriticalSection.create;
  sw := TStopWatch.create(false);
  Patching.initialize;
  Timer1.enabled := true;
  Timer2.enabled := true;
  mainpageloaded := false;
  noexecute := false;

  if (paramstr(1) = '-noexecute') then
    noexecute := true;

  SetTransparent(self, true);

  if fileexists('media/patcher/pbar_fill.png') then
  begin
    bar1ld.Picture.LoadFromFile('media/patcher/pbar_fill.png');
    bar2ld.Picture.LoadFromFile('media/patcher/pbar_fill.png');
  end;

  if fileexists('media/patcher/pbar_back.png') then
  begin
    bar1unl.Picture.LoadFromFile('media/patcher/pbar_unl.png');
    bar2unl.Picture.LoadFromFile('media/patcher/pbar_unl.png');
  end;

  if fileexists('media/patcher/background.png') then
  begin
    frame.Picture.LoadFromFile('media/patcher/background.png');
    frame.Stretch := false;
    frame.Proportional := false;
  end;

  bar1ld.Width := 0;
  bar2ld.Width := 0;
  bar1unl.Visible := false;
  bar2unl.Visible := false;
  bar1ld.Visible := false;
  bar2ld.Visible := false;

  self.BringToFront;

  pForm.Left := trunc(screen.Width / 2 - pForm.Width / 2);
  pForm.Top := trunc(screen.Height / 2 - pForm.Height / 2);
end;

procedure TpForm.frameMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

procedure TpForm.Timer1Timer(Sender: TObject);
var
  param: PChar;
  astr: string;
begin
  patcher.lock;

  pForm.bar2ld.Width :=
    trunc(pForm.bar2unl.Width * (patcher.status.completed /
    (patcher.status.filesizes + 1)));
  pForm.bar1ld.Width :=
    trunc(pForm.bar1unl.Width * (patcher.status.filework /
    (patcher.status.filesize + 1)));

  pForm.transferred.Caption := patcher.status.downloaded;
  pForm.status.Caption := patcher.status.text;

  if patcher.status.finished then
  begin
    Timer1.enabled := false;
    param := addr(astr[1]);

    param := PChar('-version:' + FloatToStr(patcher.update));

    if patcher.newlicense then
      param := PChar(param + ' -license');
    if patcher.newpatch then
      param := PChar(param + ' -patch');

    if not(noexecute) then
      ShellExecute(0, 'open', 'Game.exe', param, nil, SW_SHOWNORMAL);
    Timer1.enabled := false;
    patcher.unlock;
    Application.terminate;
  end;
  patcher.unlock;
end;

procedure processmessages;
begin
  // Application.processmessages;
end;

function RateFormat(rate: Integer): string;
// var
// rate: double;
begin
  // rate := ((AWorkCount - lastwork) * (sw.fFrequency / (sw.ElapsedTicks + 1)) / 1024);
  result := '';
  rate := round(rate / 1024);

  if rate < 1000 then
    result := FormatFloat('0', rate) + ' KB/s'
  else
    result := FormatFloat('0.00', rate / 1024) + ' MB/s';
end;

procedure TpForm.Timer2Timer(Sender: TObject);
begin
  patcher.lock;
  pForm.transferspeed.Caption := RateFormat(patcher.status.rate);
  patcher.status.rate := round(patcher.status.rate * 0.9);
  patcher.unlock;
end;

class procedure TNetWorkerGUI.IdTCPClient1Work(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
var
  delta: Int64;
begin
  try
    delta := GetTickCount - tick;

    patcher.lock;
    patcher.status.completed := patcher.status.completed + AWorkCount -
      lastwork;
    patcher.status.filework := patcher.status.filework + AWorkCount - lastwork;
    patcher.unlock;

    if delta = 0 then
      delta := 1;

    sw.Stop;

    patcher.lock;
    patcher.status.rate := patcher.status.rate + AWorkCount - lastwork;

    patcher.status.downloaded := FormatFloat('0.00', patcher.status.completed /
      1024 / 1024) + ' of ' + FormatFloat('0.00', patcher.status.filesizes /
      1024 / 1024) + 'MB';
    patcher.unlock;

    sw.Start;

    lastwork := AWorkCount;
    tick := GetTickCount;
  except
    on E: Exception do
      showmessage('TNetWorker.OnWork:' + E.Message);
  end;
end;

procedure TNetWorkerGUI.IdTCPClient1WorkBegin(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  lastwork := 0;
end;

procedure TNetWorkerGUI.IdTCPClient1WorkEnd(ASender: TObject;
  AWorkMode: TWorkMode);
begin
  lastwork := 0;
end;

begin
  netGUI := TNetWorkerGUI.create;

end.
