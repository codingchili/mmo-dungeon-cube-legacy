unit Effects_Smooth;

interface

uses Classes, Forms, Windows, Mouse_Proxy;

type
  TSmoother = class(TThread)
  protected
    procedure Execute; override;
    procedure GoSmooth();
  public
    constructor create();
  end;

procedure Initialize;

const
  resistance = 0.965;

type
  TFloatPoint = record
    X, Y: single;
  end;

var
  deltaforce, position: TFloatPoint;
  ThisMouse, LastMouse: TPoint;
  Smoother: TSmoother;

implementation

uses Main, Settings_Patcher;

procedure TSmoother.GoSmooth;
begin
  if (GetAsyncKeyState(VK_LBUTTON) <> 0) and (RenderForm.MouseInClient) then
  begin
    ThisMouse := TMouse.CursorPos;
    deltaforce.X := ThisMouse.X - LastMouse.X;
    deltaforce.Y := ThisMouse.Y - LastMouse.Y;
  end;

  position.Y := position.Y + deltaforce.Y;
  position.X := position.X + deltaforce.X;

  RenderForm.Top := round(position.Y);
  RenderForm.Left := round(position.X);

  if (abs(deltaforce.Y) > 0) then
    deltaforce.Y := deltaforce.Y * resistance;

  if (abs(deltaforce.X) > 0) then
    deltaforce.X := deltaforce.X * resistance;

  if (RenderForm.Top + deltaforce.Y < 0) or (RenderForm.Top + RenderForm.height + deltaforce.Y > screen.height) then
    deltaforce.Y := deltaforce.Y * -1;
  if (RenderForm.Left + deltaforce.X < 0) or (RenderForm.Left + RenderForm.width + deltaforce.X > screen.width) then
    deltaforce.X := deltaforce.X * -1;

  LastMouse := TMouse.CursorPos;
  // Application.ProcessMessages;
  application.HandleMessage;
end;

procedure TSmoother.Execute;
var
  tmt: TThreadMethod;
begin
  sleep(500);
  position.X := RenderForm.Left;
  position.Y := RenderForm.Top;

  if (settings.option.compact) then
    tmt := self.GoSmooth
  else
    tmt := application.HandleMessage;

  while not terminated do
  begin
    sleep(8);
    Synchronize(self, tmt);
  end;
end;

constructor TSmoother.create;
begin
  inherited create(false);
end;

procedure Initialize();
begin
  Smoother := TSmoother.create;
end;

end.
