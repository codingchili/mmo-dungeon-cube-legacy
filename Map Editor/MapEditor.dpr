program MapEditor;

{$SetPEFlags 1}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
 {$WEAKLINKRTTI ON}

{$R *.dres}

uses
  Vcl.Forms,
  MEMain in 'MEMain.pas' {window},
  Vcl.Themes,
  Vcl.Styles,
  Keyboard in 'Keyboard.pas',
  Camera in 'Camera.pas',
  Collider in 'Collider.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amakrits');
  Application.CreateForm(Twindow, window);
  Application.Run;
end.
