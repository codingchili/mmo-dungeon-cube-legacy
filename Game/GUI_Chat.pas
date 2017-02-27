unit GUI_Chat;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}
// extension for the game interface.
// the backend should use UDP, the incoming UDP steam is threaded
// - we must be thread-safe.

uses Rectarea, SyncObjs, SysUtils, {$IFDEF Win32}Windows, {$ENDIF} GUI_Engine, GUI_Element, GUI_Element_Text,
  GUI_Element_Edit, GUI_Element_Panel,
  conf_protocol, Classes, System_Keyboard, Controls, Mouse_Proxy;

CONST
  FLOAT_RATE = 0.4;
  TTL = 160;
  SYSTEM_COLOR = $FFFFFF00; // $FFCCCC20;
  PM_COLOR = $FFBF5FFF;
  CHAT_COLOR = $FFFFFFFF;
  YOU_COLOR = $FF00FFFF;
  FLOAT_BACKGROUND = $77000000;
  MESSAGE_BUFFER = 50;
  MESSAGE_LINES = 8;

type
  TFloatMessage = record
  private
    text: string[45];
    color: cardinal;
    TTL: integer;
    x, y: single;
    effect: TTextEffect;
  end;

type
  TChatMessage = record
  private
    text: string[64];
    sender: string[16];
    color: cardinal;
  end;

type
  TChatEngine = class
  public
    engine: TGUIEngine;
    procedure add(var packet: TPacket);
    procedure addMessage(sender, text: string; color: cardinal);
    procedure addFloat(text: string; x, y: integer; color: cardinal);
    procedure process();
    procedure Render;
    procedure NotifyScroll(direction: TDirection);
    procedure Logic;
    procedure Draw;
    procedure clear;
  private
    frame: TGUIPanel;
    textinput: TGUIEdit;
    textlines: array [0 .. MESSAGE_LINES] of TGUIText;
    cs: TCriticalSection;
    scrollpos: integer;
    ChatMessages: array [0 .. MESSAGE_BUFFER] of TChatMessage;
    FloatMessages: array of TFloatMessage;
    constructor Create;
    procedure Refresh;
    procedure Scroll(direction: TDirection);
  end;

procedure Initialize;

var
  ChatEngine: TChatEngine;

implementation

uses System_Log, Engine_Player, Main, System_Initializer, System_Camera, Vectors2, AsphyreTypes, Conf_Spritemap,
  Network_World, System_UtilExt;

procedure TChatEngine.clear();
var
  i: integer;
begin
  for i := 0 to MESSAGE_BUFFER do
  begin
    ChatMessages[i].text := '';
    ChatMessages[i].sender := '';
  end;

  addMessage('', '>> Running DungeonCube Client ' + Main.BUILD + ' <<', SYSTEM_COLOR);

  Refresh();
end;

procedure TChatEngine.NotifyScroll(direction: TDirection);
begin
  if (frame.GetRect.Contains(Tmouse.CursorPos)) then
    Scroll(direction);
end;

procedure TChatEngine.Scroll(direction: TDirection);
begin
  if (direction = TDirection.Down) then
  begin
    dec(scrollpos);
    if (scrollpos < 0) then
      inc(scrollpos);

    Refresh;
  end
  else
  begin
    inc(scrollpos);
    if (scrollpos + MESSAGE_LINES >= MESSAGE_BUFFER) then
      dec(scrollpos);

    Refresh;
  end;
end;

// type, sender, receiver, text
procedure TChatEngine.add(var packet: TPacket);
var
  MessageType: TMessageType;
begin
  try
    MessageType := TMessageType(StrToInt(packet.param(0)));

    case (MessageType) of
      pm:
        addMessage(packet.param(1), ': ' + packet.param(3), PM_COLOR);
      TMessageType.text:
        begin
          addFloat(packet.param(3), StrToInt(packet.param(4)), StrToInt(packet.param(5)), CHAT_COLOR);
          addMessage(packet.param(1), ': ' + packet.param(3), CHAT_COLOR);
        end;
      sys:
        addMessage('', packet.param(3), SYSTEM_COLOR);
    end;
  except
    on E: Exception do
      print('ERROR IN CHATENGINE_ADD: ' + E.Message);
  end;
end;

procedure TChatEngine.Logic();
var
  textdata: string[61];
  receiver: string[16];
  strings: TStringList;
  i: integer;
  arect: Rectarea.TRect;
begin
  if (textinput.focus) then
    keyboard.inputlock := true
  else
    keyboard.inputlock := false;

  for i := 0 to high(textlines) do
  begin
    arect := textlines[i].GetRect;

    if (arect.Contains(Tmouse.CursorPos)) and (keyboard.GetAsyncKeyState(VK_RBUTTON) <> 0) then
    begin
      textinput.focus := true;
      if (ChatMessages[scrollpos + MESSAGE_LINES - i].sender[1] <> '@') then
        textinput.text := ('@' + ChatMessages[scrollpos + MESSAGE_LINES - i].sender + ' ')
      else
        textinput.text := (ChatMessages[scrollpos + MESSAGE_LINES - i].sender + ' ');
    end;
  end;

  if (keyboard.GetAsyncKeyState(9) <> 0) and (Renderform.Focused) then
  begin
    textinput.focus := true;
    textinput.text := '';
  end;

  if (textinput.focus = true) and (keyboard.GetAsyncKeyState(VK_RETURN) <> 0) then
  begin
    textdata := textinput.text;
    if (textdata[1] = '@') then
    begin
      strings := TStringList.Create;
      System_UtilExt.Split(' ', textdata, strings);
      receiver := strings[0];
      strings.Delete(0);
      strings.Delimiter := ' ';
      if (length(strings.DelimitedText) > 0) then
      begin
        WorldConnection.SendMessage(player.name, receiver, strings.DelimitedText, 0, 0);
        addMessage(receiver, ': ' + strings.DelimitedText, PM_COLOR);
      end;
      strings.Free;
    end
    else if (length(textdata) > 0) then
    begin
      WorldConnection.SendMessage(player.name, '0', textdata,
        trunc(player.x - GameFonts.Items[0].TextWidth(textdata) / 2), trunc(player.y - 40));
      addMessage(player.name, ': ' + textdata, YOU_COLOR);
      addFloat(textdata, trunc(player.x - GameFonts.Items[0].TextWidth(textdata) / 2), trunc(player.y - 40),
        CHAT_COLOR);
    end;

    textinput.text := '';
    textinput.focus := false;
  end;

  engine.Logic;
end;

procedure TChatEngine.Draw();
begin
  engine.Draw;
end;

procedure Initialize;
begin
  ChatEngine := TChatEngine.Create;
end;

constructor TChatEngine.Create;
var
  i: integer;
begin
  cs := TCriticalSection.Create;
  scrollpos := 0;
  SetLength(FloatMessages, 0);

  engine := TGUIEngine.Create('controls.png');

  frame := TGUIPanel.Create(Pointer(engine), 0, Renderform.ClientHeight - 160, 400, 135);
  frame.draggable := true;

  textinput := TGUIEdit.Create(Pointer(engine), 8, 137, SPRITE_INPUTFIELD_DISCRETE, SPRITE_INPUTFIELD_FOCUS);
  textinput.setparent(Pointer(frame));
  textinput.maxlen := 45;

  // chatgui.add(frame);

  for i := high(textlines) downto 0 do
  begin // ugly hack to avoid 0 width
    textlines[i] := TGUIText.Create(Pointer(engine), 10, 5 + i * 14, '00000000000000000000000000000000000000000',
      $FFFFFFFF);
    textlines[i].hvcolor := $FFFFFFFF;
    textlines[i].setparent(Pointer(frame));
    textlines[i].text := '';
    // chatgui.add(textlines[i]);
  end;
  // chatgui.add(textinput);
  engine.pack;
  addMessage('', '>> Running DungeonCube Client ' + Main.BUILD + ' <<', SYSTEM_COLOR);
end;

procedure TChatEngine.Render;
var
  i: integer;
begin
  cs.Acquire;

  for i := 0 to length(ChatEngine.FloatMessages) - 1 do
    GameCanvas.FillRect(trunc(FloatMessages[i].x + cam.x - 3), trunc(FloatMessages[i].y + cam.y),
      trunc(GameFonts.Items[0].TextWidth(FloatMessages[i].text)) + 7, 16, FloatMessages[i].color and FLOAT_BACKGROUND);

  for i := 0 to length(ChatEngine.FloatMessages) - 1 do
    GameFonts.Items[0].TextOut(point2(trunc(FloatMessages[i].x + cam.x), trunc(FloatMessages[i].y + cam.y)),
      FloatMessages[i].text, cColor2(FloatMessages[i].color, FloatMessages[i].color));
  cs.release;
end;

procedure TChatEngine.process;
var
  i, len: integer;
begin
  cs.Acquire;
  len := length(FloatMessages);

  for i := 0 to len - 1 do
  begin
    FloatMessages[i].y := FloatMessages[i].y - FLOAT_RATE;

    if (FloatMessages[i].TTL < 1) then
    begin
      FloatMessages[i] := FloatMessages[len - 1];
      SetLength(FloatMessages, len - 1);
      dec(len);
    end
    else
      dec(FloatMessages[i].TTL);
  end;

  cs.release;
end;

procedure TChatEngine.Refresh();
var
  i: integer;
begin
  for i := 0 to MESSAGE_LINES do
  begin
    textlines[i].text := (ChatMessages[scrollpos + MESSAGE_LINES - i].sender + ChatMessages
      [scrollpos + MESSAGE_LINES - i].text);
    textlines[i].color := (ChatMessages[scrollpos + MESSAGE_LINES - i].color);
  end;
end;

procedure TChatEngine.addMessage(sender, text: string; color: cardinal);
var
  Pos: integer;
begin
  Pos := 0;
  cs.Acquire;
  try
    Move(ChatMessages[Pos], ChatMessages[Pos + 1], (length(ChatMessages) - 1) * sizeof(TChatMessage));
    ChatMessages[Pos].text := text;
    ChatMessages[Pos].color := color;
    ChatMessages[Pos].sender := sender;
    Refresh();
  finally
    cs.release;
  end;
end;

procedure TChatEngine.addFloat(text: string; x, y: integer; color: cardinal);
var
  Pos: integer;
begin
  cs.Acquire;
  try
    // add to live list...
    Pos := length(FloatMessages);
    SetLength(FloatMessages, Pos + 1);
    FloatMessages[Pos].text := text;
    FloatMessages[Pos].TTL := TTL;
    FloatMessages[Pos].color := color;
    FloatMessages[Pos].x := x;
    FloatMessages[Pos].y := y;
  finally
    cs.release;
  end;
end;

end.
