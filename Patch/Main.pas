unit Main;

interface

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, IdTCPClient, IdSocketHandle, ShellAPi, IdComponent;

type
  TPForm = class(TForm)
    loaded_none: TImage;
    loaded_all: TImage;
    Timer1: TTimer;
    status: TLabel;
    filename: TLabel;
    procedure loaded_noneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure loaded_allMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
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

procedure setstatustext(text: string);
procedure setprogress(percent: double);
procedure setfilename(text: string);
procedure terminate;
procedure processmessages;

var
  PForm: TPForm;
  netGUI: TNetWorkerGUI;
  tick, lastwork, starttick: Int64;
  totalwork: Integer;

implementation

{$R *.dfm}

Uses System_Vars, Patching, Networking;

procedure terminate;
begin
  Application.terminate;
end;

procedure processmessages;
begin
  Application.processmessages;
end;

procedure setprogress(percent: double);
begin
  PForm.loaded_all.Width := trunc((PForm.loaded_none.Width / 1000) * percent);
end;

procedure setfilename(text: string);
begin
  PForm.filename.Caption := text;
end;

procedure setstatustext(text: string);
begin
  PForm.status.Caption := text
end;

procedure SetTransparent(Aform: TForm; AValue: boolean);
begin
  PForm.TransparentColor := AValue;
  PForm.TransparentColorValue := PForm.Color;
end;

procedure TPForm.FormCreate(Sender: TObject);
begin
  SetTransparent(Self, True);

  PForm.Top := trunc(screen.Height / 2 - PForm.Height / 2) - 30;
  PForm.Left := trunc(screen.Width / 2 - PForm.Width / 2);
  loaded_all.Width := 0;

  screen.Cursors[0] := LoadCursor(HInstance, 'MAINCUR');
  screen.Cursor := screen.Cursors[0];

  Networking.initialize;
  Patching.initialize;
  Self.BringToFront;
end;

procedure TPForm.loaded_noneMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TPForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Patch();
end;

class procedure TNetWorkerGUI.IdTCPClient1Work(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
var
  bps: string;
  delta: Int64;
begin
  delta := GetTickCount - tick;
  totalwork := totalwork + AWorkCount - lastwork;

  if delta = 0 then
    delta := 1;

  bps := FormatFloat('0.00',
    (totalwork / (1 + (GetTickCount - starttick) / 1000)) / 1024);

  setprogress(trunc((totalwork / Patching.filesize) * 1000));
  setfilename(FormatFloat('0.00', totalwork / 1000000) + '/' +
    FormatFloat('0.00', Patching.filesize / 1000000) + 'MB  ' + bps + 'KB/s');
  setstatustext('Downloading ' + ExtractFileName(Patching.filename) + '.. ' +
    IntToStr(Patching.filecount - Patching.index) + '/' +
    IntToStr(Patching.filecount));

  Application.processmessages;
  lastwork := AWorkCount;
  tick := GetTickCount;
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

procedure TPForm.loaded_allMouseDown(Sender: TObject; Button: TMouseButton;
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

begin
  netGUI := TNetWorkerGUI.Create;

end.
