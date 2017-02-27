unit GUI_Windowed;

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

interface

uses GUI_Engine, GUI_Element_Panel, forms,
  System_UtilExt {$IFDEF Win32}, Windows{$ENDIF};

type
  TWindowedForm = class
  private
    handle: TGUIPanel;
  public
    engine: TGUIEngine;
    procedure Logic;
    procedure Draw;
    constructor create();
  end;

procedure Initialize;

var
  WindowedForm: TWindowedForm;

implementation

uses Main, System_Initializer;

procedure Initialize();
begin
  WindowedForm := TWindowedForm.create;
end;

constructor TWindowedForm.create;
begin
  engine := TGUIEngine.create('');

  handle := TGUIPanel.create(Pointer(engine), 1, 1, Initializer.swidth-2, 16);
  handle.draggable := true;

  if (renderform.WindowState = wsMaximized) then
   handle.Hide;

  engine.pack;
end;

procedure TWindowedForm.Logic();
begin
engine.Logic;
  if (renderform.WindowState = wsNormal) then
  begin
    renderform.Top := renderform.Top + handle.rect.Top - 1;
    renderform.Left := renderform.Left + handle.rect.Left - 1;
    handle.rect.top := 1;
    handle.rect.left := 1;
  end;
end;

procedure TWindowedForm.Draw();
begin
  self.Logic;
  engine.Draw;
end;

end.