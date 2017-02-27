program GLPatcher;

uses
  Vcl.Forms,
  Main in 'Main.pas' {RenderForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRenderForm, RenderForm);
  Application.Run;
end.
