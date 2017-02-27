unit MEMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, Vcl.Menus, JPeg;

const
  tile_width = 52;
  tile_height = 26;

type
  Twindow = class(TForm)
    scroll_view: TScrollBox;
    image_target: TImage;
    panel_toolbar: TPanel;
    image_mouse: TImage;
    menu: TPopupMenu;
    Exit1: TMenuItem;
    timer_camupdate: TTimer;
    Load1: TMenuItem;
    Save1: TMenuItem;
    New1: TMenuItem;
    type_select: TComboBox;
    timer_freedraw: TTimer;
    button_fill: TButton;
    combo_width: TComboBox;
    combo_height: TComboBox;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure button_fillClick(Sender: TObject);
    procedure timer_camupdateTimer(Sender: TObject);
    procedure Compile1Click(Sender: TObject);
    procedure Actions1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure scroll_viewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure image_mouseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure timer_freedrawTimer(Sender: TObject);
    procedure combo_widthChange(Sender: TObject);
    procedure combo_heightChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TClickHandler = class
  public
    class procedure OnClick(Sender: TObject);
  end;

var
  window: Twindow;
  LastMap: string;
  clickhandler: TClickHandler;
  imgs: array [0 .. 100] of TImage;

implementation

Uses Keyboard, Camera;

{$R *.dfm}

procedure Twindow.Actions1Click(Sender: TObject);
begin
  Keyboard.blocks.SaveToFile('blocks');
end;

procedure Twindow.button_fillClick(Sender: TObject);
var
  top, left, mouseX, mouseY: Integer;
begin
  mouseX := 0;

  while mouseX < 3000 do
  begin
    inc(mouseX, 32);
    mouseY := 0;
    while mouseY < 3000 do
    begin
      inc(mouseY, 32);

      top := (mouseY - mouseY mod 32) - scroll_view.top - 32;
      left := (mouseX - mouseX mod 32) - scroll_view.left - 32;

      image_mouse.left := left;
      image_mouse.top := top;

      image_target.Canvas.Draw(left - cam.X + 2, top - cam.Y + 2,
        image_mouse.Picture.Graphic);
    end;
  end;
end;

procedure Twindow.combo_heightChange(Sender: TObject);
begin
  image_target.height := StrToInt(combo_height.Text);
end;

procedure Twindow.combo_widthChange(Sender: TObject);
begin
  image_target.Width := StrToInt(combo_width.Text);
end;

procedure ListFileDir(Path: string; FileList: TStrings);
var
  SR: TSearchRec;
begin
  if FindFirst(Path + '*.png', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr <> faDirectory) then
      begin
        FileList.Add(SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure Twindow.Compile1Click(Sender: TObject);
begin
  Keyboard.blocks.SaveToFile('blocks');
end;

procedure Twindow.Exit1Click(Sender: TObject);
begin
  if MessageDlg('Exit Without Saving?', mtError, mbOKCancel, 0) = mrOK then
    window.Close;
end;

procedure Twindow.FormCreate(Sender: TObject);
var
  Tiles: TStringList;
  i: Integer;
  top: Integer;
  left: Integer;
begin
  if not directoryexists('maps') then
    mkdir('maps');

  image_target.Canvas.Brush.Color := clWhite;
  image_target.Canvas.Brush.Style := bsSolid;
  image_target.Canvas.FillRect(Rect(0, 0, 3000, 3000));

  screen.Cursors[0] := LoadCursor(HInstance, 'MAINCUR');
  screen.Cursor := screen.Cursors[0];

  Tiles := TStringList.Create;
  clickhandler := TClickHandler.Create;
  ListFileDir('tiles/', Tiles);

  left := 10;
  top := -15;

  for i := 0 to (Tiles.Count) - 1 do
  begin
    top := top + 35;

    if top > window.height - 50 then
    begin
      left := left + 36;
      top := 20;
    end;

    imgs[i] := TImage.Create(self);
    imgs[i].Parent := self;
    imgs[i].Picture.LoadFromFile('tiles/' + Tiles.Strings[i]);
    imgs[i].left := left;
    imgs[i].top := top;
    imgs[i].OnClick := TClickHandler.OnClick;
  end;

  image_mouse.Picture := imgs[0].Picture;
end;

procedure Twindow.FormResize(Sender: TObject);
begin
  scroll_view.Width := window.Width - 48 - scroll_view.left;
  scroll_view.height := window.height - panel_toolbar.height - 128;
  panel_toolbar.top := window.height - panel_toolbar.height * 2 - 15;
  panel_toolbar.left := trunc(window.Width / 2 - panel_toolbar.Width / 2);
end;

procedure Twindow.image_mouseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if GetAsyncKeyState(VK_RBUTTON) <> 0 then
    menu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;

procedure Twindow.New1Click(Sender: TObject);
begin
  if MessageDlg('Create New?', mtError, mbOKCancel, 0) = mrOK then
  begin
    image_target.Canvas.LineTo(0, 0);
    image_target.Canvas.Brush.Color := clWhite;
    image_target.Canvas.Brush.Style := bsSolid;
    image_target.Canvas.FillRect(Rect(0, 0, 2250, 2250));
  end;
end;

procedure Twindow.Save1Click(Sender: TObject);
var
  LastMap: string;
begin
  LastMap := InputBox('Save As..', 'Name', '');

  image_target.Picture.Bitmap.SaveToFile('maps/' + LastMap + '.bmp');
  Keyboard.blocks.SaveToFile(LastMap);
end;

procedure Twindow.scroll_viewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if GetAsyncKeyState(VK_RBUTTON) <> 0 then
    menu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;

procedure Twindow.Load1Click(Sender: TObject);
var
  map: string;
begin
  image_target.Canvas.LineTo(0, 0);
  image_target.Canvas.Brush.Color := clWhite;
  image_target.Canvas.Brush.Style := bsSolid;
  image_target.Canvas.FillRect(Rect(0, 0, 2250, 2250));

  map := InputBox('New Map Name', 'Enter map Name..',
    'map' + IntToStr(random(999) + 1));
  image_target.Picture.Bitmap.LoadFromFile('maps/' + map + '.bmp');
  Keyboard.blocks.LoadFromFile(map);
end;

procedure Twindow.timer_camupdateTimer(Sender: TObject);
begin
  Camera.cam.Update;
end;

function CursorPos(): TPoint;
var
  point: TPoint;
begin
  point.X := mouse.CursorPos.X - window.left;
  point.Y := mouse.CursorPos.Y - window.top;
  result := point;
end;

procedure Twindow.timer_freedrawTimer(Sender: TObject);
var
  point: TPoint;
begin
  point.Y := (CursorPos.Y - CursorPos.Y mod 32) - scroll_view.top - 32;
  point.X := (CursorPos.X - CursorPos.X mod 32) - scroll_view.left - 8;

  if (GetAsyncKeyState(VK_LBUTTON) <> 0) then
    image_target.Canvas.Draw(point.X - cam.X, point.Y - cam.Y,
      image_mouse.Picture.Graphic);

  image_mouse.top := point.Y;
  image_mouse.left := point.X;
end;

class procedure TClickHandler.OnClick(Sender: TObject);
begin
  window.image_mouse.Picture := TImage(Sender).Picture;
end;

end.
