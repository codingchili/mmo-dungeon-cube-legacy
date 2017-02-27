unit GUI_Element_TextLines;

interface

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

uses Rectarea, Controls, GUI_Engine, GUI_Element, Vectors2, AsphyreTypes,
  Classes, SysUtils;

const
  CHAR_HEIGHT = 16;
  //NEWLINE = '\'; //must be same as in conf_protocol

type
  TGUITextLines = class(TGUIElement)
  private
    rect: Rectarea.TRect;
    color: cardinal;
    textlines, buflines: TStringList;
    dirty: boolean;
  public
    textwidth, textheight: integer;
    procedure SetText(text: string);
    function TextFromFile(filename: string): boolean;
    procedure Draw(); override;
    procedure Logic(); override;
    procedure MouseEvent(var event: TGUIMouseEvent); override;
    constructor Create(engine: Pointer; left, top, width: integer; color: cardinal = TEXT_COLOR;
      hovercolor: cardinal = TEXT_COLOR);
  end;

implementation

uses System_Initializer, System_UtilExt, Conf_Protocol;

{ TGUIText }

constructor TGUITextLines.Create(engine: Pointer; left, top, width: integer; color: cardinal = TEXT_COLOR;
  hovercolor: cardinal = TEXT_COLOR);
begin
  inherited Create(engine, left, top, 0, TEXT_HEIGHT);
  self.mode := Mode_Text; // engine identifier
  self.color := color;
  textlines := TStringList.Create;
  buflines := TStringList.Create;

  TGUIEngine(engine).Add(TGUIElement(self));
end;

procedure TGUITextLines.SetText(text: string);
var
  formatted: boolean;
  left, char, i: integer;
begin
  buflines.Clear;
  formatted := false;
  // buflines := TStringList.Create();
  left := length(text);
  textwidth := 0;

  char := 0;

  for i := 0 to length(text) - 1 do
  begin
    if (text[i] = NEWLINE) then
    begin
      Split(NEWLINE, text, buflines, true);
      formatted := true;
      break;
    end;
  end;

  if not formatted then
  begin
    while (left > 60) do
    begin
      buflines.Add(Copy(text, char, 60));
      left := left - 60;
      char := char + 60;
    end;
    if (left > 0) then
      buflines.Add(Copy(text, char, left));
  end;

  for i := 0 to buflines.Count - 1 do
    if (GameFonts[0].textwidth(buflines.Strings[i]) > textwidth) then
      textwidth := round(GameFonts[0].textwidth(buflines.Strings[i]));

  // pos.left := trunc((562 / 2) - (GameFonts[0].textwidth(buflines.Strings[0]) / 2));
  // pos.top := trunc(254 / 2) - 50;
  textheight := (buflines.Count) * CHAR_HEIGHT;
  dirty := true;
end;

function TGUITextLines.TextFromFile(filename: string): boolean;
var
  handle: TextFile;
  text: string;
begin
  buflines.Clear;
  AssignFile(handle, filename);
  textwidth := 0;

  if fileexists(filename) then
  begin
    result := true;
    try
      reset(handle);

      while not eof(handle) do
      begin
        readln(handle, text);
        buflines.Add(text);

        if (GameFonts[0].textwidth(text) > textwidth) then
          textwidth := round(GameFonts[0].textwidth(text));
      end;
    finally
      closeFile(handle);
    end;
  end
  else
    result := false;

  textheight := (buflines.Count) * CHAR_HEIGHT;
  dirty := true;
end;

procedure TGUITextLines.MouseEvent(var event: TGUIMouseEvent);
begin
  //
end;

procedure TGUITextLines.Draw;
var
  i: integer;
begin
  rect := self.GetRect; // do not do logic in the draw event,

  for i := 0 to textlines.Count - 1 do
    GameFonts.Items[0].TextOut(point2(rect.left, rect.top + i * 16), textlines.Strings[i], cColor2(color));
end;

procedure TGUITextLines.Logic;
var
  i: integer;
begin
  if dirty then
  begin
    textlines.Clear;
    for i := 0 to buflines.Count - 1 do
      textlines.Add(buflines[i]);

    dirty := false;
  end;

  rect := self.GetRect;
end;

end.