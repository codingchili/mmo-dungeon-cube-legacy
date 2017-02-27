program Game;

{$IFNDEF Linux}
{$SetPEFlags 1}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$WEAKLINKRTTI ON}
{$ENDIF}

{$R *.dres}

uses
  {$IFDEF Linux}
  cthreads,
  cmem,
  interfaces,
  {$ENDIF}
  Forms,
  Main in 'Main.pas' {RenderForm},
  Engine_Player in 'Engine_Player.pas',
  System_Camera in 'System_Camera.pas',
  System_Keyboard in 'System_Keyboard.pas',
  System_Ping in 'System_Ping.pas',
  System_Initializer in 'System_Initializer.pas',
  {$IFDEF Win32}
  Vcl.Themes,
  Vcl.Styles,
  {$ENDIF}
  GUI_LoginScreen in 'GUI_LoginScreen.pas',
  GUI_Bars in 'GUI_Bars.pas',
  Network_MetaServer in 'Network_MetaServer.pas',
  Conf_Protocol in 'Conf_Protocol.pas',
  Engine_Particles in 'Engine_Particles.pas',
  Network_WorldServer in 'Network_WorldServer.pas',
  Engine_Map in 'Engine_Map.pas',
  Effects_Blending in 'Effects_Blending.pas',
  Engine_Projectiles in 'Engine_Projectiles.pas',
  Debug_Stopwatch in 'Debug_Stopwatch.pas',
  Network_World in 'Network_World.pas',
  System_Multiplayer in 'System_Multiplayer.pas',
  System_Text in 'System_Text.pas',
  System_Log in 'System_Log.pas',
  System_Animation in 'System_Animation.pas',
  Conf_Spritemap in 'Conf_Spritemap.pas',
  GUI_Chat in 'GUI_Chat.pas',
  GUI_Menu in 'GUI_Menu.pas',
  GUI_StatusBox in 'GUI_StatusBox.pas',
  GUI_SkillBar in 'GUI_SkillBar.pas',
  GUI_Inventory in 'GUI_Inventory.pas',
  Effects_Fire in 'Effects_Fire.pas',
  Effects_Lightning in 'Effects_Lightning.pas',
  System_Spells in 'System_Spells.pas',
  GUI_Targeting in 'GUI_Targeting.pas',
  Effects_Splat in 'Effects_Splat.pas',
  GUI_License in 'GUI_License.pas',
  GUI_Banner in 'GUI_Banner.pas',
  Automated_Tests in 'Automated_Tests.pas',
  System_Mouse in 'System_Mouse.pas',
  GUI_ServerSelect in 'GUI_ServerSelect.pas',
  GUI_CharSelect in 'GUI_CharSelect.pas',
  System_DayTime in 'System_DayTime.pas',
  Map_Trees in 'Map_Trees.pas',
  GUI_Windowed in 'GUI_Windowed.pas',
  GUI_SplashMain in 'GUI_SplashMain.pas',
  System_GUIStrapper in 'System_GUIStrapper.pas',
  GUI_Element in 'GUI_Element.pas',
  GUI_Element_Background in 'GUI_Element_Background.pas',
  GUI_Element_Button in 'GUI_Element_Button.pas',
  GUI_Element_Edit in 'GUI_Element_Edit.pas',
  GUI_Element_Image in 'GUI_Element_Image.pas',
  GUI_Element_Panel in 'GUI_Element_Panel.pas',
  GUI_Element_Text in 'GUI_Element_Text.pas',
  GUI_Element_TextLines in 'GUI_Element_TextLines.pas',
  GUI_Engine in 'GUI_Engine.pas',
  GUI_TextFilter in 'GUI_TextFilter.pas',
  Mouse_Proxy in 'Mouse_Proxy.pas',
  Rectarea in 'Rectarea.pas',
  System_Audio in 'System_Audio.pas',
  System_UtilExt in 'System_UtilExt.pas',
  System_CipherIO in 'System_CipherIO.pas',
  Effects_Smoke in 'Effects_Smoke.pas',
  GUI_MiniMap in 'GUI_MiniMap.pas',
  Effects_Water in 'Effects_Water.pas',
  System_Items in 'System_Items.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TRenderForm, RenderForm);
  Application.Run;
end.