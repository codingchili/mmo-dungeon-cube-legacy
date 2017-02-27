program GLPatcher;

uses
  Vcl.Forms,
  Main in 'Main.pas' {RenderForm},
  GUI_Element_Panel in 'GUI_Element_Panel.pas',
  GUI_Element_Text in 'GUI_Element_Text.pas',
  GUI_Element_TextLines in 'GUI_Element_TextLines.pas',
  GUI_Engine in 'GUI_Engine.pas',
  GUI_TextFilter in 'GUI_TextFilter.pas',
  Mouse_Proxy in 'Mouse_Proxy.pas',
  Rectarea in 'Rectarea.pas',
  System_Audio in 'System_Audio.pas',
  System_Mouse in 'System_Mouse.pas',
  System_UtilExt in 'System_UtilExt.pas',
  GUI_Element in 'GUI_Element.pas',
  GUI_Element_Background in 'GUI_Element_Background.pas',
  GUI_Element_Button in 'GUI_Element_Button.pas',
  GUI_Element_Edit in 'GUI_Element_Edit.pas',
  GUI_Element_Image in 'GUI_Element_Image.pas',
  Debug_Stopwatch in 'Debug_Stopwatch.pas',
  System_Keyboard in 'System_Keyboard.pas',
  Conf_Spritemap in 'Conf_Spritemap.pas',
  System_Text in 'System_Text.pas',
  Effects_Blending in 'Effects_Blending.pas',
  GUI_Patcher in 'GUI_Patcher.pas',
  Effects_Fire in 'Effects_Fire.pas',
  Engine_Particles in 'Engine_Particles.pas',
  Patching in 'Patching.pas',
  Settings_Patcher in 'Settings_Patcher.pas',
  Effects_Smooth in 'Effects_Smooth.pas',
  Conf_Protocol in 'Conf_Protocol.pas',
  System_Log in 'System_Log.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRenderForm, RenderForm);
  Application.Run;
end.
