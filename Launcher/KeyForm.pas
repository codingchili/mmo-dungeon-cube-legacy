unit KeyForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Image1: TImage;
    keyedit: TEdit;
    procedure keyeditClick(Sender: TObject);
    procedure keyeditChange(Sender: TObject);
    procedure keyeditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;
  Key: string;
  registered: boolean;
  keyready: boolean;

implementation

uses Patching;

{$R *.dfm}

procedure SaveKey();
var
  keyfile: textFile;
begin
  AssignFile(keyfile, 'conf/Key.dat');
  rewrite(keyfile);
  writeln(keyfile, Key);
  closefile(keyfile);
end;

procedure loadkey();
var
  keyfile: textFile;
begin
  if fileexists('conf/Key.dat') then
  begin
    AssignFile(keyfile, 'conf/key.dat');
    Reset(keyfile);
    readln(keyfile, Key);
    closefile(keyfile);
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  Form3.Top := trunc(screen.Height / 2 - Form3.Height / 2);
  Form3.Left := trunc(screen.Width / 2 - Form3.Width / 2);
  registered := false;
  keyready := false;

  if not fileexists('conf/key.dat') then
    self.Visible := true
  else
  begin
    self.Visible := false;
    loadkey();
  end;
end;

procedure TForm3.Image1MouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TForm3.keyeditChange(Sender: TObject);
begin
  if length(keyedit.Text) = 25 then
  begin
    Key := keyedit.Text;
    keyready := true;
  end;
end;

procedure TForm3.keyeditClick(Sender: TObject);
begin
  keyedit.Clear;
end;

procedure TForm3.keyeditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (getasynckeystate(VK_ESCAPE) <> 0) then
    Form3.Close;
end;

end.
