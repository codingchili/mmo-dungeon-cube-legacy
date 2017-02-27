program Launcher;

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {pForm},
  Vcl.Themes,
  Vcl.Styles,
  Patching in 'Patching.pas',
  Utils in 'Utils.pas',
  Stopwatch in 'Stopwatch.pas',
  System_Log in 'System_Log.pas',
  System_UtilExt in 'System_UtilExt.pas',
  Conf_Protocol in 'Conf_Protocol.pas',
  System_Vars in 'System_Vars.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TpForm, pForm);
  Application.Run;

end.
