program Patcher;


{$SetPEFlags 1}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
 {$WEAKLINKRTTI ON}

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {PForm},
  Vcl.Themes,
  Vcl.Styles,
  System_Vars in 'System_Vars.pas',
  Patching in 'Patching.pas',
  Networking in 'Networking.pas',
  Utils in 'Utils.pas',
  Conf_Protocol in 'Conf_Protocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPForm, PForm);
  Application.Run;
end.
